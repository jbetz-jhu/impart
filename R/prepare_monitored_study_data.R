#' Prepare monitored study data for monitoring and analysis
#'
#' @param data A \code{data.frame} containing the baseline covariates, treatment
#' assignment, randomization time, observed outcomes, and times of outcome
#' assessments.
#' @param study_time A \code{numeric} scalar indicating the time from study
#' initiation to the export of the data (or latest observed event, such as
#' randomization of participants or outcome ascertainment).
#' @param id_variable A \code{character} scalar containing the column name of
#' the study participant identifier.
#' @param covariates_variables A \code{character} vector containing the column
#' names of the covariates and outcomes observed prior to randomization.
#' @param enrollment_time_variable A \code{character} scalar containing the
#' column name of the enrollment/randomization time variable.
#' @param treatment_variable A \code{character} scalar containing the
#' column name of the binary treatment assignment variable.
#' @param outcome_variables A \code{character} vector containing the column
#' names of the outcomes observed after randomization.
#' @param outcome_time_variables A \code{character} vector containing the column
#' names of the observation times of outcomes observed after randomization.
#' @param observe_missing_times A \code{numeric} a vector containing the time
#' from randomization to the closing of each study window when
#' \code{time_to_event == FALSE}.
#' @param outcomes_sequential A \code{logical} scalar: are outcomes sequential
#' in nature (e.g. a sequence of study visits) or not (e.g. times from
#' randomization to different events)?
#' @param time_to_event A \code{logical} scalar: are outcomes times from
#' randomization to events (TRUE) or a continuous, binary, or ordinal outcome
#' (FALSE)?
#'
#' @return a \code{list} containing the prepared data, the original data,
#' and the variables specified above.
#' @export
#'
#' @examples
#' # To be added

prepare_monitored_study_data <-
  function(
    data,
    study_time,
    id_variable,
    covariates_variables,
    enrollment_time_variable,
    treatment_variable,
    outcome_variables,
    outcome_time_variables,
    observe_missing_times = NULL,
    outcomes_sequential = TRUE,
    time_to_event = FALSE
  ){
    if(any(!is.finite(study_time), !is.numeric(study_time))){
      stop("`study_time` must be numeric and not missing.")
    }

    missing_vars <-
      setdiff(
        x = c(id_variable, covariates_variables, enrollment_time_variable,
              outcome_variables, outcome_time_variables),
        y = names(data)
      )

    if(length(missing_vars) > 0){
      stop("Variables not contained in dataset: ",
           paste(missing_vars, collapse = ", "))
    }

    latest_event <-
      unlist(data[, c(enrollment_time_variable, outcome_time_variables)]) |>
      max(na.rm = TRUE)

    if(latest_event > study_time){
      stop("Latest enrollment/outcome event > `study_time`: ",
           latest_event, " > ", study_time)
    }

    n_outcomes <- length(outcome_variables)
    n_outcome_times <- length(outcome_time_variables)

    if(n_outcomes != n_outcome_times){
      stop("Number of outcomes (", n_outcomes, ") must be equal to the number ",
           "of outcome measurement times (", n_outcome_times, ").")
    }

    if(!time_to_event){
      if(is.null(observe_missing_times)){
        stop("If `time_to_event` is FALSE, then `observe_missing_times` must ",
             "be specified.")
      } else {
        if(length(observe_missing_times) != n_outcomes){
          stop("Number of outcomes (", n_outcomes, ") must be equal to the ",
               "length of the times at which missingness is observed (",
               length(observe_missing_times), ").")
        }
      }
    } else {
      if(!is.null(observe_missing_times)){
        stop("If `time_to_event` is TRUE, then `observe_missing_times` must ",
             "be `NULL`.")
      }
    }

    all_ids <- unique(data[, id_variable])

    # Check data for potential errors: Missing or Duplicated IDs
    if(any(is.na(data[, id_variable]))){
      stop("ID variable `", id_variable, "` cannot contain NA values. ",
           "NA values observed in rows: ",
           paste(which(is.na(data[, id_variable])), collapse = ", "))
    }

    if(any(duplicated(data[, id_variable]))){
      stop("ID variable `", id_variable, "` cannot contain duplicates. ",
           "Duplicate values observed in rows: ",
           paste(which(duplicated(data[, id_variable])), collapse = ", "))
    }

    # Check for missing randomization times
    if(any(is.na(data[, enrollment_time_variable]))){
      stop(
        "Participants missing entry time in rows: ",
        which(is.na(data[, enrollment_time_variable])) |>
          paste(collapse = ", ")
      )
    }

    # Check that all outcome variables for time-to-event are binary
    if(time_to_event){
      if(!all(unlist(data[, outcome_variables]) %in% c(0:1))){
        stop("All variables in `outcome_variables` must be binary for time-to-event outcomes")
      }

      if(!all(unlist(data[, outcome_time_variables]) > 0)){
        stop("All outcome times must be greater than 0.")
      }
    }

    if(outcomes_sequential & n_outcomes > 1){
      # Check for inconsistencies in Outcome Timing
      outcome_time_differences <-
        data[, utils::tail(x = outcome_time_variables, -1)] -
        data[, utils::head(x = outcome_time_variables, -1)]

      inconsistencies <-
        which(x = outcome_time_differences < 0, arr.ind = TRUE) |>
        as.data.frame()

      if(nrow(inconsistencies) > 0){
        visits_affected <- inconsistencies$col
        msg <- "Inconsistent timing in outcomes: "
        for(i in visits_affected){
          rows_affected <- subset(x = inconsistencies, col == i)$row
          msg <-
            paste0(msg, "`", names(outcome_time_differences)[i], "`: rows (",
                   paste(rows_affected, collapse = ", "), "); ")
        }

        stop(msg)
      }
    }

    # Check for Outcomes with Missing Outcome Times
    missing_outcome_times <-
      which(
        x = !is.na(data[, outcome_variables]) &
          is.na(data[, outcome_time_variables]),
        arr.ind = TRUE
      ) |>
      data.frame()

    if(nrow(missing_outcome_times) > 0){
      stop("Missing outcome times in rows: ",
           paste0(unique(missing_outcome_times$row), collapse = ", "))
    }

    wide_data <-
      list(
        ".id" = data[, id_variable],
        data[, c(covariates_variables, treatment_variable)]
      )

    enrollment_time <- wide_data[[".e"]] <- data[, enrollment_time_variable]

    enrollment_to_study_time <- study_time - enrollment_time

    for(i in 1:n_outcomes){
      observed_time_i <- data[, outcome_time_variables[i]]
      outcome_i <- data[, outcome_variables[i]]

      if(time_to_event){
        outcome_indicator <-
          1*(!(is.na(observed_time_i) | is.na(observed_time_i)))
        if(any(is.na(observed_time_i)) | any(is.na(outcome_i))){
          stop("Event times and indicators should not contain missing values.")
        }
      } else {
        missing_outcome_times_i <-
          which(is.na(observed_time_i) &
                  enrollment_to_study_time > observe_missing_times[i])

        observed_time_i[missing_outcome_times_i] <-
          enrollment_time[missing_outcome_times_i] + observe_missing_times[i]

        enrollment_to_outcome <- observed_time_i - enrollment_time

        outcome_indicator <- rep(NA, length(outcome_i))
        outcome_indicator[which(!is.na(outcome_i))] <- TRUE
        outcome_indicator[
          which(is.na(outcome_i) & !is.na(observed_time_i))
        ] <- FALSE
        outcome_indicator[
          which(enrollment_to_outcome > observe_missing_times[i] &
                  is.na(outcome_i))
        ] <- FALSE

        observed_time_i[
          which(
            is.na(observed_time_i) &
              study_time - enrollment_time > observe_missing_times[i]
          )
        ]
      }

      wide_data[[outcome_variables[i]]] <- data[, outcome_variables[i]]
      wide_data[[paste0(".t_", i)]] <- observed_time_i
      wide_data[[paste0(".r_", i)]] <- 1*outcome_indicator
    }


    prepared_data <-
      list(
        data = do.call(what = cbind, args = wide_data),
        original_data = data,
        variables = list(
          id_variable = id_variable,
          covariates_variables = covariates_variables,
          enrollment_time_variable = enrollment_time_variable,
          treatment_variable = treatment_variable,
          outcome_variables = outcome_variables,
          outcome_time_variables = outcome_time_variables,
          observe_missing_times = observe_missing_times,
          outcomes_sequential = outcomes_sequential,
          time_to_event = time_to_event,
          renamed_outcome_times = paste0(".t_", 1:n_outcomes),
          outcome_indicators = paste0(".r_", 1:n_outcomes)
        ),
        study_time = study_time,
        observe_missing_times = observe_missing_times,
        outcomes_sequential = outcomes_sequential,
        time_to_event = time_to_event
      )

    class(prepared_data) <- c("monitored_study_data", class(prepared_data))

    return(prepared_data)
  }
