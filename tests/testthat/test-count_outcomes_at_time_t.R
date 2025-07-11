test_that(
  desc = "Continuous Outcome Check - 50%",
  code = {

    continuous_last_event_time <-
      example_1[, c(".enrollment_time", ".t_1", ".t_2", ".t_3", ".t_4")] |>
      unlist() |>
      max(na.rm = TRUE) |>
      ceiling()

    example_1_prepared_final_data <-
      prepare_monitored_study_data(
        data = example_1,
        study_time = continuous_last_event_time,
        id_variable = ".id",
        covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
        enrollment_time_variable = ".enrollment_time",
        treatment_variable = "tx",
        outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
        outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
        # Observe missingness 1 week after target study visit
        observe_missing_times = c(30, 60, 90, 120) + 7
      )

    # Continuous Outcome Check - 50%
    expect_no_condition(
      object =
        count_outcomes_at_time_t(
          prepared_data = example_1_prepared_final_data,
          study_time = continuous_last_event_time/1:4
        )
    )
  }
)


test_that(
  desc = "Time-to-Event Outcome",
  code = {
    tte_last_event_time <-
      with(
        data = sim_colon_cancer,
        expr = enroll_time + years_to_death
      ) |>
      max()

    tte_last_event_time <- tte_last_event_time + 1/365.25

    colon_cancer_active_prepared <-
      prepare_monitored_study_data(
        data = sim_colon_cancer |> data.frame(),
        study_time = tte_last_event_time,
        id_variable = ".id",
        covariates_variables =
          c("age", "sex", "obstruction", "perforation", "organ_adherence",
            "positive_nodes", "differentiation", "local_spread",
            "time_surgery_registration"),
        enrollment_time_variable = "enroll_time",
        treatment_variable = "arm",
        outcome_variables = "event_death",
        outcome_time_variables = "years_to_death",
        observe_missing_times = NULL,
        time_to_event = TRUE
      )

    expect_no_condition(
      object =
        count_outcomes_at_time_t(
          prepared_data = colon_cancer_active_prepared,
          study_times = tte_last_event_time/1:4
        )
    )
  }
)
