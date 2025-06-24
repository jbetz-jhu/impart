#' Reconstruct the count of events at a given study time during a study
#'
#' This function uses the timing of events, such as randomization times and
#' times of outcomes, to reconstruct the data available at an earlier point in
#' the study. This can be used for monitoring the rate of information accrual
#' over the course of a study. This returns the number of events at a particular
#' point during the study.
#'
#' @param prepared_data A prepeared dataset: see
#' [impart::prepare_monitored_study_data]
#' @param study_times A \code{numeric} vector of study times at which to count
#' events.
#'
#' @return A \code{data.frame}
#' @export
#'
#' @examples
#' # To be added

count_outcomes_at_time_t <-
  function(
    prepared_data,
    study_times
  ){

    if(any(study_times > prepared_data$study_time)){
      stop("`study_times` (", paste(study_times, collapse = ", "), ") should ",
           "all be less than current study time (i.e. ",
           "`prepared_data$study_time` = ", prepared_data$study_time, ")")
    }
    data <- prepared_data$data
    outcome_time_variables <- prepared_data$variables$renamed_outcome_times
    outcome_variables <- prepared_data$variables$outcome_variables

    time_to_event <- prepared_data$time_to_event

    if(time_to_event){
      event_names <- prepared_data$variables$outcome_variables
    } else {
      event_names <- c("randomization", prepared_data$variables$outcome_variables)
    }

    outcomes_at_time_t <-
      count_outcomes(
        prepared_data = prepared_data,
        study_time = max(study_times)
      )

    outcome_counts <- list()

    for(i in 1:length(study_times)){
      study_time_i <- study_times[i]
      event_times_i <-
        aggregate(
          time ~ event,
          FUN = function(x, t_i = study_time_i) x[max(which(x < t_i))],
          data = outcomes_at_time_t
        )

      outcome_counts[[i]] <-
        merge(
          x = outcomes_at_time_t,
          y = event_times_i
        )

      outcome_counts[[i]]$time <- study_time_i
    }

    outcome_counts <-
      do.call(what = rbind, args = outcome_counts)

    return(
      outcome_counts[with(data = outcome_counts, expr = {order(time, event)}),]
    )
  }
