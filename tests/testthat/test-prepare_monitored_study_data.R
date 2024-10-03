test_that(
  desc = "Error Handling Works - Continuous/Binary",
  code = {
    expect_error(
      object =
        prepare_monitored_study_data(
          data = example_1,
          study_time = Inf,
          id_variable = "..id",
          covariates_variables = c(".x_1", ".x_2", ".x_3", ".x_4"),
          enrollment_time_variable = "..enrollment_time",
          treatment_variable = c(".tx"),
          outcome_variables = c(".y_1", ".y_2", ".y_3", ".y_4"),
          outcome_time_variables = c("..t_1", "..t_2", "..t_3", "..t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "Variables not contained"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data = example_1,
          study_time = 200,
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "Latest enrollment"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data = example_1,
          study_time = ceiling(max(example_1$.t_4)),
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "Number of outcomes"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data = example_1,
          study_time = ceiling(max(example_1$.t_4)),
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "Number of outcomes"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data =
            within(
              data = example_1,
              expr = {.id[1:3] <- 1}
            ),
          study_time = ceiling(max(example_1$.t_4)),
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "cannot contain duplicates"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data =
            within(
              data = example_1,
              expr = {.id[1:3] <- NA}
            ),
          study_time = ceiling(max(example_1$.t_4)),
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "cannot contain NA values"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data =
            within(
              data = example_1,
              expr = {.enrollment_time[1:3] <- NA}
            ),
          study_time = ceiling(max(example_1$.t_4)),
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "missing entry time"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data =
            within(
              data = example_1,
              expr = {.t_1[1:3] <- .t_2[1:3] + 100}
            ),
          study_time = ceiling(max(example_1$.t_4)),
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "Inconsistent timing"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data =
            within(
              data = example_1,
              expr = {.t_1[1:3] <- NA}
            ),
          study_time = ceiling(max(example_1$.t_4)),
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        ),
      regexp = "Missing outcome times"
    )
  }
)


test_that(
  desc = "Error Handling Works - Time-to-Event",
  code = {
    expect_error(
      object =
        prepare_monitored_study_data(
          data =
            within(
              data = test_data,
              expr = {tte_1[1:3] <- -tte_1[1:3]}
            ),
          study_time = 20,
          id_variable = ".id",
          covariates_variables = c("x_1"),
          enrollment_time_variable = "enrollment",
          treatment_variable = c("tx"),
          outcome_variables = c("event_1", "event_2"),
          outcome_time_variables = c("tte_1", "tte_2"),
          observe_missing_times = c(0, 0),
          outcomes_sequential = FALSE,
          time_to_event = TRUE
        ),
      regexp = "All outcome times must be greater than 0"
    )

    expect_error(
      object =
        prepare_monitored_study_data(
          data =
            within(
              data = test_data,
              expr = {event_1[1:3] <- 5}
            ),
          study_time = 20,
          id_variable = ".id",
          covariates_variables = c("x_1"),
          enrollment_time_variable = "enrollment",
          treatment_variable = c("tx"),
          outcome_variables = c("event_1", "event_2"),
          outcome_time_variables = c("tte_1", "tte_2"),
          observe_missing_times = c(0, 0),
          outcomes_sequential = FALSE,
          time_to_event = TRUE
        ),
      regexp = "All variables in `outcome_variables`"
    )
  }
)


test_that(
  desc = "Test Continuous, Binary, Time-to-Event Data",
  code = {
    expect_no_condition(
      object =
        example_1_prepared <-
        prepare_monitored_study_data(
          data = example_1,
          study_time = ceiling(max(example_1$.t_4)),
          id_variable = ".id",
          covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
          enrollment_time_variable = ".enrollment_time",
          treatment_variable = c("tx"),
          outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
          outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
          observe_missing_times = c(30, 60, 90, 120) + 7,
          outcomes_sequential = TRUE,
          time_to_event = FALSE
        )
    )

    expect_no_condition(
      object =
        prepare_monitored_study_data(
          data = test_data,
          study_time = 20,
          id_variable = ".id",
          covariates_variables = c("x_1"),
          enrollment_time_variable = "enrollment",
          treatment_variable = c("tx"),
          outcome_variables = c("event_1", "event_2"),
          outcome_time_variables = c("tte_1", "tte_2"),
          observe_missing_times = c(0, 0),
          outcomes_sequential = FALSE,
          time_to_event = TRUE
        )
    )
  }
)
