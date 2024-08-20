#' Reconstruct a trajectory of information accrual
#'
#' While [impart::estimate_information()] provides an estimated information
#' level at a single point, investigators may want to visualize a trajectory
#' of how quickly information is accruing as participants are randomized and
#' their outcomes are obtained. \code{information_trajectory} attempts to
#' reconstruct the study data available at different points in time, and then
#' compute the information accrued at these times. Regression models can be used
#' to smooth these trajectories to provide projections of when the information
#' will reach pre-specified thresholds.
#'
#' @param prepared_data A prepeared dataset: see
#' [impart::prepare_monitored_study_data]
#' @param monitored_design An object of class \code{monitored_design} created
#' using [impart::initialize_monitored_design()]
#' @param estimation_function A function whose arguments include a data.frame
#' named \code{data}
#' @param estimation_arguments A list of any additional arguments needed by
#' \code{estimation_function}
#' @param correction_function An optional function to adjust the variance
#' estimate using parameters from \code{estimation_arguments}
#' @param orthogonalize Logical scalar: Should estimates, their covariance,
#' and the resulting test statistics be orthogonalized?
#' @param n_min A \code{numeric} scalar indicating the minimum sample size for
#' the information trajectory
#' @param n_increment A \code{numeric} scalar indicating the increment in sample
#' size for calculating the trajectory from `n_min` to the current sample size
#' in `prepared_data`.
#' @param rng_seed Numeric scalar containing the L'Ecuyer pseudorandom
#' number generator seed
#' @param control A list containing the control arguments for computation,
#' typically created with [impart::monitored_analysis_control()]
#'
#' @return A \code{data.frame} containing the information and number of outcome
#' events at each analysis
#'
#' @export
#'
#' @examples
#' # To be added

information_trajectory <-
  function(
    prepared_data,
    monitored_design = NULL,
    estimation_function,
    estimation_arguments,
    correction_function = NULL,
    orthogonalize,
    n_min = 30,
    n_increment = 5,
    rng_seed,
    control = monitored_analysis_control()
  ){
    outcome_counts <-
      count_outcomes(
        prepared_data =
          data_at_time_t(
            prepared_data = prepared_data,
            study_time = prepared_data$study_time
          )
      )

    primary_outcome <-
      utils::tail(x = prepared_data$variables$outcome_variables, n = 1)

    # If a previous analysis has been conducted, ensure n_min isn't below the
    # number of outcomes at the previous interim analysis.
    if(!is.null(monitored_design)){
      latest_analysis <- monitored_design[[length(monitored_design)]]

      n_previous <- sum(!is.na(latest_analysis$data[primary_outcome]))
      if(n_min < n_previous){
        stop("`n_min` (", n_min, ") is at or below the number of complete ",
             "outcomes at the previous analysis (", n_previous, ").")
      }

      information_target <-
        monitored_design$original_design$information_target
    } else {
      information_target <- NA
    }

    primary_outcome_counts <-
      outcome_counts[which(outcome_counts$event == primary_outcome),]

    n_current <- max(primary_outcome_counts$count_complete, na.rm = TRUE)

    if(n_min >= n_current){
      stop("`n_min` (", n_min, ") is at or below number of complete outcomes (",
           n_current, ").")
    }

    information_times <-
      primary_outcome_counts[
        which(
          primary_outcome_counts$count_complete %in%
            seq(from = n_min, to = n_current, by = n_increment)
            ),
        ]$time

    information <-
      data.frame(
        count_outcomes_at_time_t(
          prepared_data = prepared_data,
          study_times = information_times
        )$count_complete,
        information = NA
      )

    for(i in 1:nrow(information)){
      data_i <-
        data_at_time_t(
          prepared_data = prepared_data,
          study_time = information$times[i]
        )

      information_i <-
        estimate_information(
          data = data_i$data,
          monitored_design = monitored_design,
          estimation_function = estimation_function,
          estimation_arguments = estimation_arguments,
          correction_function = correction_function,
          orthogonalize = orthogonalize,
          rng_seed = rng_seed,
          control = control,
          return_results = FALSE
        )

      information$information[i] <-
        if(orthogonalize){
          utils::tail(x = information_i$information_orthogonal, n = 1)
        } else {
          utils::tail(x = information_i$information, n = 1)
        }
    }

    information$information_lag_1 <-
      with(information, c(NA, utils::head(x = information, n = -1)))

    information$information_change =
      with(information, information - information_lag_1)

    information$information_pct_change =
      with(information, 100*information_change/information)

    information$information_fraction <-
      information$information/information_target

    return(information)
  }
