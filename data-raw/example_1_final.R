library(impart)

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

# Load Data: Revert to 1600 days study time
data("example_1", package = "impart")

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
    observe_missing_times = c(30, 60, 90, 120) + 7
  )


# Conduct 1st Interim Analysis
data("example_1_ia_1", package = "impart")

interim_analysis_1 <-
  monitored_analysis(
    data = impart::example_1_ia_1,
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
    correction_function = standardization_correction
  )

interim_analysis_1$interim_analysis_1$decision
interim_analysis_1$interim_analysis_1$decision_data
interim_analysis_1$interim_analysis_1$information_fraction_orthogonal


# Conduct 2nd Interim Analysis
data("example_1_ia_2", package = "impart")

interim_analysis_2 <-
  monitored_analysis(
    data = impart::example_1_ia_2,
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
    correction_function = standardization_correction
  )

interim_analysis_2$interim_analysis_2$decision
interim_analysis_2$interim_analysis_2$decision_data
interim_analysis_2$interim_analysis_2$information_fraction_orthogonal


# Determine Timing of Final Analysis
data_t_1600 <-
  data_at_time_t(
    prepared_data = prepared_final_data,
    study_time = 1600
  )

data_t_1600_prepared <-
  prepare_monitored_study_data(
    data = data_t_1600$data,
    study_time = last_event,
    id_variable = ".id",
    covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
    enrollment_time_variable = ".e",
    treatment_variable = "tx",
    outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
    outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
    observe_missing_times = c(30, 60, 90, 120) + 7
  )


# Plot/Count Outcomes
plot_outcome_counts(
  prepared_data = data_t_1600_prepared
)

count_outcomes_at_time_t(
  prepared_data = data_t_1600_prepared,
  study_times = c(788, 1194, 1600)
)

# Compute Information Trajectory
data_t_1600_trajectory <-
  information_trajectory(
    prepared_data = data_t_1600_prepared,
    monitored_design = interim_analysis_2,
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
    n_min = 143,
    n_increment = 3,
    rng_seed = 23456,
    control = monitored_analysis_control()
  )

# Plot/Smooth Trajectory
plot(
  information ~ y_4,
  data = data_t_1600_trajectory
)

abline(
  lm(
    formula = information ~ y_4,
    data = data_t_1600_trajectory
  ),
  lty = 1
)

# Requires `deming` package
abline(
  deming::theilsen(
    formula = information ~ y_4,
    data = data_t_1600_trajectory
  ),
  lty = 3
)

abline(
  h = monitored_design$original_design$information_thresholds,
  lty = 2
)

subset(
  x =
    count_outcomes(
      prepared_data = data_t_1600_prepared
    ),
  event == "y_4" & count_complete == 166
)

data_n_final <-
  data_at_time_t(
    prepared_data = prepared_final_data,
    study_time = 1499
  )

example_1_final <- data_n_final$data


final_analysis <-
  monitored_analysis(
    data = example_1_final,
    monitored_design = interim_analysis_2,
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
    correction_function = standardization_correction
  )

final_analysis$final_analysis$decision
final_analysis$final_analysis$decision_data
final_analysis$final_analysis$information_fraction_orthogonal


usethis::use_data(example_1_final, overwrite = TRUE)
