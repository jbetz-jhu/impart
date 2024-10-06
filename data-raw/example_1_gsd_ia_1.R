library(impart)

# Universal Study Design Parameters
minimum_difference <- 5 # Effect Size: Difference in Means of 5 or greater
alpha <- 0.05 # Type I Error Rate
power <- 0.9 # Statistical Power
test_sides <- 2 # Direction of Alternatives

# Compute Fixed Sample Size Requirement
fixed_n_sd_10 <-
  pwr::pwr.t.test(
    d = minimum_difference/10,
    sig.level = alpha,
    power = power
  )

# Adjust for Missing Data
pr_missing <- 0.2 # 20% missing outcomes
fixed_n_total <- 2*ceiling(fixed_n_sd_10$n)/(1 - pr_missing)


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

# Inflate the sample size requirements to account for multiple testing
# Obtain inflation factor to preserve Type I Error: adjust fixed sample size
inflation_factor <-
  rpact::getDesignCharacteristics(trial_design)$inflationFactor

group_sequential_n_total <- ceiling(fixed_n_total*inflation_factor)

# Determine sample sizes when analyses will take place
group_sequential_n_analyses <-
  ceiling(information_rates*group_sequential_n_total)

# Load Data: Revert to 800 days study time
data("example_1", package = "impart")

example_1_gsd <-
  example_1[c(1: group_sequential_n_total),]

last_event <-
  example_1_gsd[, c(".enrollment_time", ".t_1", ".t_2", ".t_3", ".t_4")] |>
  unlist() |>
  max(na.rm = TRUE) |>
  ceiling()

prepared_final_data <-
  prepare_monitored_study_data(
    data = example_1_gsd,
    study_time = last_event,
    id_variable = ".id",
    covariates_variables = c("x_1", "x_2", "x_3", "x_4"),
    enrollment_time_variable = ".enrollment_time",
    treatment_variable = "tx",
    outcome_variables = c("y_1", "y_2", "y_3", "y_4"),
    outcome_time_variables = c(".t_1", ".t_2", ".t_3", ".t_4"),
    observe_missing_times = c(30, 60, 90, 120) + 7
  )

plot_outcome_counts(
  prepared_data = prepared_final_data
)

abline(h = group_sequential_n_analyses)
abline(v = c(1055, 1620))

count_outcomes_at_time_t(
  prepared_data = prepared_final_data,
  study_time = c(1055, 1620)
)

data_n_interim_1 <-
  data_at_time_t(
    prepared_data = prepared_final_data,
    study_time = 1055
  )

example_1_gsd_ia_1 <- data_n_interim_1$data

usethis::use_data(example_1_gsd_ia_1, overwrite = TRUE)
