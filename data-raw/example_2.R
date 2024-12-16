library(impart)

source(file.path(getwd(), "data-raw", "data_generating_mechanisms.R"))

# Universal Study Design Parameters: Binary Outcome
minimum_difference <- 0.15 # Effect Size: Risk Difference of 15% or greater
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




### Simulate Trial Data ########################################################
set.seed(seed = 1013)

# Data Generating Mechanism Parameters
n_covariates <- 4
enrollment_per_day <- 0.15
visit_times <- c(30, 60, 90, 120)
visit_window <- c(-14, 14)

treatment_effect <- 5.5

r_squared_random_effects <- 0.125
r_squared_covariates <- 0.20
total_variance <- 100
total_sd <- sqrt(total_variance)

pr_dropout <- c(0.15, 0.05, 0.025, 0.025)
pr_complete_study <- tail(x = cumprod(1 - pr_dropout), n = 1)

re_variance <- r_squared_random_effects*total_variance
covariate_variance <- r_squared_covariates*total_variance
residual_variance <- total_variance - (re_variance + covariate_variance)


# Translate information into approximate sample size for given value of
# nuisance parameters
approximate_sample_size <-
  information_to_n_continuous_1_to_1(
    information = information_adaptive,
    sigma_0 = total_sd,
    sigma_1 = total_sd,
    round_up = TRUE
  )

n_participants = ceiling(1.25*approximate_sample_size$n_total/pr_complete_study)

sigma_x <- diag(1, n_covariates)

lme_beta <-
  setNames(
    object = c(rnorm(n = n_covariates)),
    nm = c(paste0("x_", 1:n_covariates))
  )

covariate_variance_unscaled <- as.numeric(t(lme_beta) %*% sigma_x %*% lme_beta)

beta_scale <- sqrt(covariate_variance/covariate_variance_unscaled)

lme_beta <- c(lme_beta*beta_scale, "tx" = treatment_effect)


data_wide <-
  sim_lme_trial(
    data =
      rnd_block(
        data =
          independent_mvn(
            n = n_participants,
            mean_x = rep(0, n_covariates),
            sigma_x = sigma_x
          )
      ),
    visit_times = visit_times,
    mean_outcomes = c(0, 0, 0, 0),
    outcome_cov =
      list(
        y_1 = lme_beta,
        y_2 = lme_beta,
        y_3 = lme_beta,
        y_4 = lme_beta
      ),
    re_variance = re_variance,
    re_correlation = NULL,
    residual_sd = sqrt(residual_variance),
    pr_dropout = pr_dropout,
    dropout_cov = NULL
  )

data_wide <-
  data.frame(`.id` = as.character(1:nrow(data_wide)), data_wide)

data_wide$.enrollment_time <-
  rexp(n = n_participants, rate = enrollment_per_day) |> base::cumsum()

outcomes <-
  names(data_wide[grep(pattern = "^y_obs_\\d{1,}$", x = names(data_wide))])

data_wide[paste0(".t_", 1:length(outcomes))] <- NA

for(i in 1:length(outcomes)){
  observed_rows <- which(!is.na(data_wide[outcomes[i]]))

  data_wide[observed_rows, paste0(".t_", i)] <-
    runif(
      n = length(observed_rows),
      min = pmax(visit_times[i] + visit_window[1], 0),
      max = visit_times[i] + visit_window[2]
    )

  missing_rows <- which(is.na(data_wide[outcomes[i]]))
  data_wide[missing_rows, paste0(".t_", i)] <-
    visit_times[i] + visit_window[2]

  data_wide[paste0(".t_", i)] <-
    data_wide$.enrollment_time + data_wide[paste0(".t_", i)]
}

keep_cols <-
  grep(
    pattern = "(\\.id|\\.\\w{1,}_time|x_\\d{1,}|tx|y_obs_\\d{1,}|\\.t_\\d{1,})",
    x = names(data_wide)
  )

outcome_columns <-
  names(data_wide)[grep(pattern = "(y_obs_\\d{1,})", x = names(data_wide))]

data_wide <-
  data_wide[, keep_cols]

names(data_wide) <-
  gsub(x = names(data_wide), pattern = "y_obs_(\\d)", replacement = "y_\\1")

example_1 <- data_wide

usethis::use_data(example_1, overwrite = TRUE)
