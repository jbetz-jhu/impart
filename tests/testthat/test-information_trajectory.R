test_that(
  desc = "Check Error Handling",
  code = {
    # Universal Study Design Parameters
    minimum_difference <- 5 # Effect Size: Difference in Means of 5 or greater
    alpha <- 0.05 # Type I Error Rate
    power <- 0.9 # Statistical Power
    test_sides <- 2 # Direction of Alternatives

    # Determine information required to achieve desired power at fixed error rate
    information_single_stage <-
      impart::required_information_single_stage(
        delta = minimum_difference,
        alpha = alpha,
        power = power
      )

    # Group Sequential Design Parameters
    information_rates <-
      c(0.50, 0.75, 1.00) # Analyses at 50%, 75%, and 100% of the Total Information
    type_of_design <- "asOF" # O'Brien-Fleming Alpha Spending
    type_beta_spending <- "bsOF" # O'Brien-Fleming Beta Spending

    # Set up group sequential testing procedure
    trial_design <-
      rpact::getDesignGroupSequential(
        alpha = alpha,
        beta = 1 - power,
        sided = 2,
        informationRates = information_rates,
        typeOfDesign = type_of_design,
        typeBetaSpending = type_beta_spending,
        bindingFutility = FALSE
      )

    # Inflate information level to account for multiple testing
    information_adaptive <-
      impart::required_information_sequential(
        information_single_stage = information_single_stage,
        trial_design = trial_design
      )

    # Initialize the monitored design
    monitored_design <-
      initialize_monitored_design(
        trial_design = trial_design,
        null_value = 0,
        maximum_sample_size = 280,
        information_target = information_adaptive,
        orthogonalize = TRUE,
        rng_seed_analysis = 54321
      )

    # Obtain time of last event
    last_event <-
      example_1[, c(".enrollment_time", ".t_1", ".t_2", ".t_3", ".t_4")] |>
      unlist() |>
      max(na.rm = TRUE) |>
      ceiling()

    prepared_final_data <-
      prepare_monitored_study_data(
        data = example_1,
        study_time = last_event,
        id_variable = ".id",
        covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
        enrollment_time_variable = ".enrollment_time",
        treatment_variable = "tx",
        outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
        outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
        # Observe missingness 1 week after target study visit
        observe_missing_times = c(30, 60, 90, 120) + 7
      )

    example_1_counts <-
      count_outcomes(
        prepared_data = prepared_final_data
      )

    prepared_final_data <-
      prepare_monitored_study_data(
        data = example_1,
        study_time = last_event,
        id_variable = ".id",
        covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
        enrollment_time_variable = ".enrollment_time",
        treatment_variable = "tx",
        outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
        outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
        # Observe missingness 1 week after target study visit
        observe_missing_times = c(30, 60, 90, 120) + 7
      )

    # Reconstruct the data when N = 70 final outcomes were obtained
    data_n_final_70 <-
      data_at_time_t(
        prepared_data = prepared_final_data,
        study_time =
          # Time when 70 final outcomes are observed:
          ceiling(
            subset(
              example_1_counts,
              event == "y_4" & count_complete == 70
            )$time
          )
      )

    expect_error(
      object = {
        information_trajectory(
          prepared_data = data_n_final_70,
          monitored_design = monitored_design,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = TRUE,
          n_min = 80,
          n_increment = 10,
          rng_seed = 23456,
          control = monitored_analysis_control_testing()
        )
      },
      regex = "`n_min` \\(\\d{1,}\\) is at or below number of complete outcomes"
    )

    expect_no_condition(
      object = {
        data_n_70_trajectory <-
          information_trajectory(
            prepared_data = data_n_final_70,
            monitored_design = monitored_design,
            estimation_function = standardization,
            estimation_arguments =
              list(
                estimand = "difference",
                y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
                y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
                family = gaussian,
                treatment_column = "tx",
                outcome_indicator_column = ".r_4"
              ),
            correction_function = standardization_correction,
            orthogonalize = TRUE,
            n_min = 40,
            n_increment = 10,
            rng_seed = 23456,
            control = monitored_analysis_control_testing()
          )
      }
    )
  }
)
