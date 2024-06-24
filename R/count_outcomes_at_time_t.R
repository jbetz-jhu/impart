#' Reconstruct the count of events at a given time during a study
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

    data <- prepared_data$data
    outcome_time_variables <- prepared_data$variables$renamed_outcome_times
    outcome_variables <- prepared_data$variables$outcome_variables

    event_names <- c("randomization", prepared_data$variables$outcome_variables)
    count_total <-
      data.frame(
        times = study_times
      )
    count_total[, event_names] <- NA
    count_complete <- count_total

    for(i in 1:length(study_times)){
      for(j in 1:length(event_names)){
        if(j == 1){
          count_total[i, event_names[j]] <-
            count_complete[i, event_names[j]] <-
            sum(data$`.e` <= study_times[i], na.rm = TRUE)
        } else {
          count_total[i, event_names[j]] <-
            sum(data[, outcome_time_variables[j - 1]] <=
                  study_times[i], na.rm = TRUE)
          count_complete[i, event_names[j]] <-
            sum(data[, outcome_time_variables[j - 1]] <= study_times[i] &
                  !is.na(data[, outcome_variables[j - 1]]), na.rm = TRUE)
        }
      }
    }

    return(
      list(
        count_total = count_total,
        count_complete = count_complete
      )
    )
  }
