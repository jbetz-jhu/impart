test_that(
  desc = "multiplication works",
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

    expect_no_condition(
      object = {
        count_outcomes_at_time_t(
          prepared_data = prepared_final_data,
          study_times = c(50, 100, 150, 200)
        )
      }
    )
  }
)
