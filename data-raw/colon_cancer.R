# Colon Cancer Data: DOI: 10.1056/NEJM199002083220602
set.seed(12345)

library(survival)
library(dplyr)
library(tidyr)
library(mice)

colon_cancer <-
  survival::colon %>%
  dplyr::select(
    `.id` = id,
    arm = rx,
    age, sex,
    obstruction = obstruct,
    perforation = perfor,
    organ_adherence = adhere,
    positive_nodes = nodes,
    differentiation = differ,
    local_spread = extent,
    time_surgery_registration = surg,
    event = status,
    time_to = time,
    event_type = etype
  ) %>%
  # Convert from Long to Wide
  tidyr::pivot_longer(
    cols = all_of(x = c("event", "time_to"))
  ) %>%
  dplyr::mutate(
    event_type =
      case_when(
        event_type == 1 ~ "recurrence",
        event_type == 2 ~ "death"
      )
  ) %>%
  tidyr::unite(
    col = name,
    name, event_type
  ) %>%
  tidyr::pivot_wider(
    names_from = name,
    values_from = value
  )


### Label Factors, Convert Timescale to Years ##################################
colon_cancer <-
  colon_cancer %>%
  # Label factor variables
  dplyr::mutate(
    across(
      .cols = c("obstruction", "perforation", "organ_adherence"),
      .fns = function(x)
        factor(
          x = x,
          levels = 0:1,
          labels = c("0. No", "1. Yes"),
        )
    ),

    arm =
      factor(
        x = arm,
        levels = c("Obs", "Lev", "Lev+5FU")
      ),

    sex =
      factor(
        x = sex,
        levels = 0:1,
        labels = c("0. Female", "1. Male"),
      ),

    differentiation =
      factor(
        x = differentiation,
        levels = 1:3,
        labels = c("1. Well", "2. Moderate", "3. Poor"),
      ),

    local_spread =
      factor(
        x = local_spread,
        levels = 1:4,
        labels =
          c("1. Submucosa", "2. Muscle",
            "3. Serosa", "4. Contiguous structures"),
      ),

    time_surgery_registration =
      factor(
        x = time_surgery_registration,
        levels = 0:1,
        labels =
          c("0. Short", "1. Long")
      ),

    years_to_death = time_to_death/365.25,
    years_to_recurrence = time_to_recurrence/365.25,
    time_to_death = NULL,
    time_to_recurrence = NULL
  )


# Save the original dataset prior to imputation
colon_cancer_original <- colon_cancer

# Determine Columns with Missingness
# which(colSums(is.na(colon_cancer)) > 0)

# Fit Cox Model
cox_death_adjusted <-
  survival::coxph(
    formula =
      survival::Surv(time = years_to_death, event = event_death) ~
      arm + age + sex + obstruction + perforation + organ_adherence +
      local_spread + time_surgery_registration,
    data = colon_cancer
  )

colon_cancer$cox_expected <-
  predict(
    object = cox_death_adjusted,
    newdata =
      within(
        data = colon_cancer,
        expr = {arm = "Obs"}
      ),
    type = "expected"
  )

### Impute Missing Covariates ##################################################
# Imputation of covariates involves model-based prediction of expected survival
# under observation given covariates without missingness
colon_cancer_impute <-
  colon_cancer %>%
  dplyr::select(
    dplyr::all_of(
      x = c("age", "sex", "obstruction", "perforation",
            "organ_adherence", "differentiation", "local_spread",
            "time_surgery_registration", "positive_nodes", "cox_expected")
    )
  )

colon_cancer_impute_0 <-
  mice::mice(
    data = colon_cancer_impute,
    m = 1,
    maxit = 0
  )

predictor_matrix <- colon_cancer_impute_0$predictorMatrix
predictor_matrix[, "local_spread"] <- 0

### Perform MICE ###############################################################
colon_cancer_mice <-
  mice::mice(
    data = colon_cancer_impute,
    predictorMatrix = predictor_matrix,
    # Single Imputation
    m = 1,
    # 20 Iterations of MICE Algorithm
    maxit = 20,
    # Seed for reproducibility
    seed = 12345,
    printFlag = FALSE
  )

# plot(colon_cancer_mice)


# Get completed data
colon_cancer_mice <-
  complete(colon_cancer_mice) %>%
  dplyr::select(
    differentiation, positive_nodes
  )


### Assemble Completed Dataset #################################################
colon_cancer_imputed <-
  dplyr::bind_cols(
    colon_cancer %>%
      dplyr::select(
        -dplyr::all_of(x = names(colon_cancer_mice))
      ),
    colon_cancer_mice
  ) %>%
  as.data.frame() %>%
  dplyr::select(
    dplyr::any_of(
      x = names(colon_cancer_original)
    )
  )


# Subset colon cancer data to active treatment arms: Lev, Lev+5FU
colon_cancer_active <-
  subset(
    x = colon_cancer_imputed,
    subset = arm %in% c("Lev+5FU", "Lev")
  )

colon_cancer_active$tx <-
  with(
    data = colon_cancer_active,
    1*(arm == "Lev+5FU") + 0*(arm == "Lev")
  )


power <- 0.90
alpha <- 0.05
test_sides <- 2
# O'Brien-Fleming Alpha Spending
efficacy_bounds <- "asOF"

# Originally designed for 90% Power to detect HR of ~1.35 with One-Sided Test
events_single_stage <-
  impart::hr_design(
    events = NULL,
    hazard_ratio = 0.74,
    power = power,
    alpha = alpha,
    test_sides = test_sides
  )

events_1s <- ceiling(events_single_stage)

info_fractions_75_100 <- c(.75, 1)

design_2s_of_75_100 <-
  rpact::getDesignGroupSequential(
    alpha = alpha,
    sided = test_sides,
    informationRates = info_fractions_75_100,
    typeOfDesign = efficacy_bounds
  )

if_2s_of_75_100 <-
  rpact::getDesignCharacteristics(
    design = design_2s_of_75_100
  )$inflationFactor

events_2s_of_75_100 <-
  (events_single_stage*info_fractions_75_100*if_2s_of_75_100) |>
  ceiling()

# Add 10% increase in number of events
events_2s_of_75_100 <- events_2s_of_75_100*1.10

# Get number of participants to yield target events
n_participants <-
  with(
    data = colon_cancer_active,
    expr = length(event_death)*max(events_2s_of_75_100)/sum(event_death)
  ) |>
  ceiling()

n_resample <- n_participants - nrow(colon_cancer_active)

additional_participants <-
  colon_cancer_active[
    sample(x = 1:nrow(colon_cancer_active), size = n_resample),
  ]

additional_participants$.id <-
  additional_participants$.id + 1000


colon_cancer_active <-
  rbind(
    colon_cancer_active,
    additional_participants
  )

# Add simulated recruitment time: March 1984 to October 1987
enrollment_duration <-
  difftime(
    time1 = as.Date("1987-10-31"),
    time2 = as.Date("1984-03-01"),
    units = "days"
  ) |>
  as.numeric()

colon_cancer_active$enroll_time <-
  runif(
    n = nrow(colon_cancer_active),
    min = 0,
    max = enrollment_duration/365.25
  ) |>
  sort()

trial_end <-
  with(
    data = colon_cancer_active,
    expr = {
      c(max(enroll_time + years_to_death),
        max(enroll_time + years_to_recurrence)) |>
        max()
    }
  ) + 1/356.25

sim_colon_cancer <- colon_cancer_active

usethis::use_data(sim_colon_cancer, overwrite = TRUE)

sim_colon_cancer_prepared <-
  impart::prepare_monitored_study_data(
    data = sim_colon_cancer,
    study_time = trial_end,
    id_variable = ".id",
    covariates_variables =
      c("age", "sex", "obstruction", "perforation", "organ_adherence",
        "positive_nodes", "differentiation", "local_spread",
        "time_surgery_registration"),
    enrollment_time_variable = "enroll_time",
    treatment_variable = "tx",
    outcome_variables = "event_death",
    outcome_time_variables = "years_to_death",
    observe_missing_times =  NULL,
    time_to_event = TRUE
  )

plot_outcome_counts(
  prepared_data = sim_colon_cancer_prepared,
  study_time = trial_end,
  type = "e"
)

abline(h = events_1s)
abline(v =  6.9)

count_outcomes_at_time_t(
  prepared_data = sim_colon_cancer_prepared,
  study_times = 6.9
)
