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
