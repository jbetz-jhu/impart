#' Reconstruct data available at an earlier point in a study
#'
#' This function uses the timing of events, such as randomization times and
#' times of outcomes, to reconstruct the data available at an earlier point in
#' the study. This can be used for monitoring the rate of information accrual
#' over the course of a study.
#'
#' @param prepared_data A prepeared dataset: see
#' [impart::prepare_monitored_study_data]
#' @param study_time A \code{numeric} scalar indicating the study time at which
#' the data should be reconstructed
#'
#' @return A list
#' @export
#'
#' @examples
#' # To be added

data_at_time_t <-
  function(
    prepared_data,
    study_time
  ){

    if(!"monitored_study_data" %in% class(prepared_data)){
      stop("`prepared_data` must come from `prepare_monitored_study_data`")
    }

    time_to_event <- prepared_data$time_to_event

    data <- prepared_data$data

    # 1. Subset to participants enrolled by study time
    data <- data[which(data$`.e` <= study_time),]

    outcome_times <- prepared_data$variables$outcome_time_variables
    renamed_outcome_times <- prepared_data$variables$renamed_outcome_times


    wide_data <-
      list(
        data[, c(".id", prepared_data$variables$covariates_variables,
                 prepared_data$variables$treatment_variable, ".e")]
      )

    enrollment_time <- data$`.e`

    outcome_variables <- prepared_data$variables$outcome_variables
    observe_missing_times <- prepared_data$variables$observe_missing_times

    for(i in 1:length(outcome_times)){
      outcome_i <- outcome_i_at_time_t <- data[, outcome_variables[i]]

      if(time_to_event){
        observed_time <- data[, renamed_outcome_times[i]] + enrollment_time
        outcome_i_at_time_t <- data[, outcome_variables[i]]

        # Censor events after study_time
        outcome_i_at_time_t[which(outcome_i %in% 1 &
                                    observed_time > study_time)] <- 0

        observed_time_at_time_t <-
          pmin(observed_time, study_time) - enrollment_time

        wide_data[[outcome_variables[i]]] <- outcome_i_at_time_t
        wide_data[[outcome_times[i]]] <- observed_time_at_time_t

        wide_data[[paste0(".t_", i)]] <- observed_time_at_time_t
        wide_data[[paste0(".r_", i)]] <- observed_time <= study_time
      } else {
        observed_time <- data[, renamed_outcome_times[i]]
        enrollment_to_outcome <- observed_time - enrollment_time
        not_yet_observed <-
          is.na(observed_time) | (observed_time > study_time)

        is_observed_complete <-
          !(is.na(outcome_i) | not_yet_observed)

        is_observed_missing <-
          is.na(outcome_i) & (!not_yet_observed) &
          (enrollment_to_outcome >= c(observe_missing_times)[i])

        is_not_yet_observed <-
          (
            not_yet_observed | # Observed after study_time threshold
              (is.na(outcome_i) & (!not_yet_observed) &
                 (enrollment_to_outcome < c(observe_missing_times)[i]))
          )

        outcome_determined <-
          (is_observed_complete + is_observed_missing + is_not_yet_observed)

        outcome_undetermined <- !(outcome_determined %in% 1)

        if(any(outcome_undetermined)){
          stop("Undetermined outcomes in rows: ",
               paste0(which(outcome_undetermined), collapse = ", "))
        }

        outcome_indicator <- NA*is_observed_complete
        outcome_indicator[which(is_observed_complete)] <- 1
        outcome_indicator[which(is_observed_missing)] <- 0

        wide_data[[paste0(".r_", i)]] <- outcome_indicator

        observed_time[is_not_yet_observed] <- NA
        wide_data[[paste0(".t_", i)]] <- pmin(observed_time, study_time)

        outcome_i_at_time_t[which(is_not_yet_observed)] <- NA
        wide_data[[outcome_variables[i]]] <- outcome_i_at_time_t
      }
    }

    prepared_data$data <- do.call(what = cbind, args = wide_data)
    prepared_data$study_time <- study_time

    return(prepared_data)
  }
