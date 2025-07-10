test_that(
  desc = "Continuous Outcome",
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
              outcome_formula_treatment = y_4 ~ x_1 + x_2 + x_3 + x_4,
              outcome_formula_control = y_4 ~ x_1 + x_2 + x_3 + x_4,
              family = gaussian,
              treatment_column = "tx"
            ),
          correction_function = standardization_df_adjust_tsiatis_2008,
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
                outcome_formula_treatment = y_4 ~ x_1 + x_2 + x_3 + x_4,
                outcome_formula_control = y_4 ~ x_1 + x_2 + x_3 + x_4,
                family = gaussian,
                treatment_column = "tx"
              ),
            correction_function = standardization_df_adjust_tsiatis_2008,
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


test_that(
  desc = "Time-To-Event Outcome",
  code = {
    # Universal Study Design Parameters
    minimum_difference <- log(0.74)
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
      c(0.75, 1.00) # Analyses at 50%, 75%, and 100% of the Total Information
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

    # Initialize the monitored design
    monitored_design <-
      initialize_monitored_design(
        trial_design = trial_design,
        null_value = 0,
        maximum_sample_size = nrow(sim_colon_cancer),
        information_target = information_adaptive,
        orthogonalize = TRUE,
        rng_seed_analysis = 54321
      )

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
        treatment_variable = "tx",
        outcome_variables = "event_death",
        outcome_time_variables = "years_to_death",
        observe_missing_times = NULL,
        time_to_event = TRUE
      )

    unadjusted_count_at_analyses <-
      information_to_events_log_hr(
        information = information_rates*information_adaptive
      )$total_events

    plot_outcome_counts(
      prepared_data = colon_cancer_active_prepared,
      type = "e"
    )
    abline(h = unadjusted_count_at_analyses)

    sim_colon_cancer_events <-
      count_outcomes(
        prepared_data = colon_cancer_active_prepared
      )

    event_times <-
      subset(
        sim_colon_cancer_events,
        count_events %in% unadjusted_count_at_analyses
      )

    # Reconstruct the data when N = 70 final outcomes were obtained
    data_for_trajectory <-
      data_at_time_t(
        prepared_data = colon_cancer_active_prepared,
        study_time = event_times$time[1]
      )

    expect_no_condition(
      object = {
        information_trajectory(
          prepared_data = data_for_trajectory,
          monitored_design = monitored_design,
          estimation_function = speffsurv_impart,
          estimation_arguments =
            list(
              formula =
                survival::Surv(time = years_to_death, event = event_death) ~
                age + sex + obstruction + perforation + organ_adherence +
                positive_nodes + differentiation + local_spread,
              treatment_column = "tx"
            ),
          correction_function = NULL,
          orthogonalize = TRUE,
          bootstrap = FALSE,
          n_min = event_times$count_events[1] - 50,
          n_increment = 10,
          rng_seed = 23456
        )
      }
    )
  }
)
