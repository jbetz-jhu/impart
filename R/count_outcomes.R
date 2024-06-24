#' Count outcome events from prepared study data
#'
#' This function uses the timing of events, such as randomization times and
#' times of outcomes, to reconstruct the data available at an earlier point in
#' the study. This can be used for monitoring the rate of information accrual
#' over the course of a study. This function returns the count of outcomes by
#' study time.
#'
#' @param prepared_data A prepeared dataset: see
#' [impart::prepare_monitored_study_data]
#' @param study_time A \code{numeric} scalar indicating the study time at which
#' the data should be reconstructed and events should be counted
#'
#' @return A \code{data.frame} in long format, with one row per individual per
#' event (i.e. randomization, outcomes).
#' @export
#'
#' @examples
#' # To be added

count_outcomes <-
  function(
    prepared_data,
    study_time = NULL
  ){

    if(!is.null(study_time)){
      prepared_data <-
        data_at_time_t(
          prepared_data = prepared_data,
          study_time = study_time
        )
    }

    data <- prepared_data$data

    data <-
      data[rank(x = data$`.e`, ties.method = "first"),]

    id_variable <- prepared_data$variables$id_variable
    outcome_variables <- prepared_data$variables$outcome_variables
    outcome_time_variables <- prepared_data$variables$renamed_outcome_times

    binary_outcome <-
      all(unlist(data[, outcome_variables]) %in% c(NA, 0, 1))

    long_times <-
      list(
        data.frame(
          id = data[, id_variable],
          event = "randomization",
          value = NA,
          time = data$`.e`,
          count_total =
            rank(
              x = data$`.e`,
              na.last = "keep",
              ties.method = "first"
            )
        )
      )

    long_times[[1]]$count_complete <- long_times[[1]]$count_total

    if(binary_outcome){
      long_times[[1]]$count_events <- NA
    }


    for(i in 1:length(outcome_variables)){
      complete_times <- data[, outcome_time_variables[i]]

      if(binary_outcome){
        event_times <- complete_times
        event_times[which(!(data[, outcome_variables[i]] %in% 1))] <- NA
      }

      complete_times[which(is.na(data[, outcome_variables[i]]))] <- NA

      event_times_i <-
        data.frame(
          id = data[, id_variable],
          event = outcome_variables[i],
          value = data[, outcome_variables[i]],
          time = data[, outcome_time_variables[i]],
          count_total =
            rank(
              x = data[, outcome_time_variables[i]],
              na.last = "keep",
              ties.method = "first"
            ),
          count_complete =
            rank(
              x = complete_times,
              na.last = "keep",
              ties.method = "first"
            )
        )

      if(binary_outcome){
        event_times_i$count_events <-
          rank(
            x = event_times,
            na.last = "keep",
            ties.method = "first"
          )
      }

      long_times[[length(long_times) + 1]] <-
        event_times_i[order(event_times_i$time),]
    }

    long_times <-
      do.call(
        what = rbind,
        args = long_times
      )

    long_times$event <-
      factor(
        x = long_times$event,
        levels = c("randomization", outcome_variables)
      )

    long_times[, c("id", "value")] <- NULL

    return(
      long_times[which(!is.na(long_times$time)),]
    )
  }
