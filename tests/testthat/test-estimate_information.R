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




### Unadjusted Analysis - No Orthogonalization #################################
unadjusted_oblique_1 <-
  tryCatch(
    expr = {
      estimate_information(
        data = example_1_ia_1,
        monitored_design = NULL,
        estimation_function = standardization,
        estimation_arguments =
          list(
            estimand = "difference",
            y0_formula = y_4 ~ 1,
            y1_formula = y_4 ~ 1,
            family = gaussian,
            treatment_column = "tx",
            outcome_indicator_column = ".r_4"
          ),
        correction_function = standardization_correction,
        orthogonalize = FALSE,
        rng_seed = 12345,
        control = monitored_analysis_control_testing()
      )
    },
    error = {"error"},
    warning = {"warning"},
    finally = {}
  )



### Unadjusted Analysis - No Orthogonalization #################################
unadjusted_oblique_2 <-
  tryCatch(
    expr = {
      interim_analysis_1 <-
        monitored_analysis(
          data = example_1_ia_1,
          monitored_design = monitored_design,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ 1,
              y1_formula = y_4 ~ 1,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = FALSE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )

      estimate_information(
        data = example_1_ia_2,
        monitored_design = interim_analysis_1,
        estimation_function = standardization,
        estimation_arguments =
          list(
            estimand = "difference",
            y0_formula = y_4 ~ 1,
            y1_formula = y_4 ~ 1,
            family = gaussian,
            treatment_column = "tx",
            outcome_indicator_column = ".r_4"
          ),
        correction_function = standardization_correction,
        orthogonalize = FALSE,
        rng_seed = 12345,
        control = monitored_analysis_control_testing()
      )
    },
    error = {"error"},
    warning = {"warning"},
    finally = {}
  )




### Unadjusted Analysis - No Orthogonalization #################################
adjusted_orthogonal_1 <-
  tryCatch(
    expr = {
      estimate_information(
        data = example_1_ia_1,
        monitored_design = NULL,
        estimation_function = standardization,
        estimation_arguments =
          list(
            estimand = "difference",
            y0_formula = y_4 ~ 1,
            y1_formula = y_4 ~ 1,
            family = gaussian,
            treatment_column = "tx",
            outcome_indicator_column = ".r_4"
          ),
        correction_function = standardization_correction,
        orthogonalize = TRUE,
        rng_seed = 12345,
        control = monitored_analysis_control_testing()
      )
    },
    error = {"error"},
    warning = {"warning"},
    finally = {}
  )



### Adjusted Analysis - Orthogonalization ######################################
adjusted_orthogonal_2 <-
  tryCatch(
    expr = {
      interim_analysis_1 <-
        monitored_analysis(
          data = example_1_ia_1,
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
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )

      estimate_information(
        data = example_1_ia_2,
        monitored_design = interim_analysis_1,
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
        orthogonalize = FALSE,
        rng_seed = 12345,
        control = monitored_analysis_control_testing()
      )
    },
    error = {"error"},
    warning = {"warning"},
    finally = {}
  )


test_that(
  desc = "Unadjusted Not Orthogonalized Works",
  code = {
    expect_contains(
      object = names(unadjusted_oblique_1),
      expected =
        c("estimates", "covariance_uncorrected", "variance", "information")
    )

    expect_contains(
      object = names(unadjusted_oblique_2),
      expected =
        c("estimates", "covariance_uncorrected", "variance", "information")
    )
  }
)

test_that(
  desc = "Adjusted Orthogonalized Works",
  code = {
    expect_contains(
      object = names(adjusted_orthogonal_1),
      expected =
        c("estimates", "covariance_uncorrected", "variance", "information")
    )

    expect_contains(
      object = names(adjusted_orthogonal_2),
      expected =
        c("estimates", "covariance_uncorrected", "variance", "information")
    )
  }
)
