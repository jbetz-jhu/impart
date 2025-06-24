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

    # Revert to data at earlier point in study, if applicable
    if(!is.null(study_time)){
      prepared_data <-
        data_at_time_t(
          prepared_data = prepared_data,
          study_time = study_time
        )
    } else {
      study_time <- prepared_data$study_time
    }

    time_to_event <- prepared_data$time_to_event

    data <- prepared_data$data
    data <- data[rank(x = data$`.e`, ties.method = "first"),]

    id_variable <- prepared_data$variables$id_variable
    outcome_variables <- prepared_data$variables$outcome_variables
    outcome_time_variables <- prepared_data$variables$renamed_outcome_times

    binary_outcome <-
      all(unlist(data[, outcome_variables]) %in% c(NA, 0, 1))

    if(time_to_event){
      event_times_list <- list()

      for(i in 1:length(outcome_variables)){

        all_events <-
          rbind(
            data.frame(
              event_name = "randomization",
              id = data$`.id`,
              time = data$`.e`,
              randomization_t = 1,
              event_t = 0,
              censor_t = 0,
              admin_t = 0
            ),
            data.frame(
              event_name = outcome_variables[i],
              id = data$`.id`,
              time = data$`.e` + data[, paste0(".t_", i)],
              randomization_t = 0,
              event_t = data[, outcome_variables[1]],
              censor_t =
                (1 - data[, outcome_variables[1]]) & data[, paste0(".r_", i)],
              admin_t =
                (1 - data[, outcome_variables[1]]) & !data[, paste0(".r_", i)]
            )
          )

        all_events <-
          tapply(
            X = all_events,
            INDEX = all_events$time,
            FUN = function(x){
              data.frame(
                time = unique(x$time),
                randomization_t = sum(x$randomization_t),
                event_t = sum(x$event_t),
                censor_t = sum(x$censor_t),
                admin_t = sum(x$admin_t)
              )
            }
          ) |>
          do.call(what = rbind)

        all_events <-
          all_events[order(all_events$time),]

        events_summary <-
          with(
            data = all_events,
            expr =
              data.frame(
                event =
                  factor(
                    x = outcome_variables[i],
                    levels = outcome_variables
                  ),
                time = time,
                count_randomized = cumsum(randomization_t),
                count_events = cumsum(event_t),
                count_censored = cumsum(censor_t),
                count_admin = cumsum(admin_t)
              )
          )

        events_summary$count_complete <-
          with(
            data = events_summary,
            expr = {(count_events + count_censored)}
          )

        events_summary$count_at_risk <-
          with(
            data = events_summary,
            expr = {(count_randomized - count_complete)}
          )

        event_times_list[[i]] <- events_summary
        names(event_times_list)[i] <- outcome_variables[i]
      }

      event_times_list <-
        do.call(
          what = rbind,
          args = event_times_list
        )

      rownames(event_times_list) <- NULL

      return(event_times_list)

    } else {

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

      long_times[[1]]$count_events <-
        long_times[[1]]$count_complete <-
        long_times[[1]]$count_total

      for(i in 1:length(outcome_variables)){
        # Reorder data by outcome time: Remove rows with missing outcome times
        data_i <-
          data[
            order(
              x = data[, outcome_time_variables[i]],
              na.last = NA
            ),
          ]

        event_time_data_i <-
          data.frame(
            id = data_i[, id_variable],
            event = outcome_variables[i],
            value = data_i[, outcome_variables[i]],
            time = data_i[, outcome_time_variables[i]],
            count_total = 1:nrow(data_i),
            count_complete = cumsum(data_i[, paste0(".r_", i)] %in% 1),
            count_events =
              if(binary_outcome){
                cumsum(data_i[, outcome_variables[i]] %in% 1)
              } else {
                NA
              }
          )

        long_times[[length(long_times) + 1]] <- event_time_data_i
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
  }
