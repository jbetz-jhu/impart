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


### Impute Missing Covariates ##################################################
# NOTE: In an attempt to preserve covariate-outcome relationships,
# a (year of event x event indicator) interaction is included as a categorical
# variable in imputation. One issue is that of the 929 participants in the
# trial, very few (929 - 915 = 14) are censored prior to the 5th year of
# follow-up, and sparsity also occurs after 7 years of follow-up. When imputing,
# those N=14 censored before year 5 are dropped, and time-to-event is top-coded
# at 7 years.

colon_cancer_impute <-
  colon_cancer %>%
  dplyr::filter(
    years_to_death >= 5 | event_death == 1
  ) %>%
  dplyr::mutate(
    # Coarsen time scale to years:
    years_to_death_ceiling = ceiling(years_to_death),
    years_to_recurrence_ceiling = ceiling(years_to_recurrence),
    years_to_death_topcode =
      case_when(
        years_to_death < 7 ~ years_to_death_ceiling,
        years_to_death >= 7 ~ 7
      )
  ) %>%
  dplyr::select(
    dplyr::all_of(
      x = c(".id", "arm", "age", "sex", "obstruction", "perforation",
            "organ_adherence", "differentiation", "local_spread",
            "time_surgery_registration", "positive_nodes",
            "years_to_death_topcode", "event_death")
    )
  ) %>%
  dplyr::mutate(
    `.id` = as.character(`.id`),
    death_time =
      factor(
        x = paste0(event_death, ":", years_to_death_topcode),
      ),
    event_death = NULL,
    years_to_death_topcode = NULL
  )

# Massive Imputation
colon_cancer_predictor_matrix <-
  matrix(
    data = 1,
    nrow = ncol(colon_cancer_impute),
    ncol = ncol(colon_cancer_impute),
  )

diag(colon_cancer_predictor_matrix) <- 0

# Do not use "id" as predictor
colon_cancer_predictor_matrix[
  , which(names(colon_cancer_impute) %in% c(".id"))
] <- 0


### Perform MICE ###############################################################
colon_cancer_mice <-
  mice::mice(
    data = colon_cancer_impute,
    predictorMatrix = colon_cancer_predictor_matrix,
    exclude = ".id",
    # Single Imputation
    m = 1,
    # 20 Iterations of MICE Algorithm
    maxit = 20,
    # Seed for reproducibility
    seed = 12345,
    printFlag = FALSE
  )

plot(colon_cancer_mice)


# Get completed data for the N=915
colon_cancer_mice <-
  complete(colon_cancer_mice) %>%
  dplyr::mutate(
    `.id` = as.numeric(`.id`)
  ) %>%
  dplyr::select(
    `.id`, differentiation, positive_nodes
  )


### Assemble Completed Dataset #################################################
colon_cancer_original <- colon_cancer

colon_cancer <-
  dplyr::full_join(
    x =
      colon_cancer %>%
      dplyr::select(
        -all_of(x = c('differentiation', "positive_nodes"))
      ),
    y =
      dplyr::bind_rows(
        colon_cancer_mice,
        colon_cancer %>%
          dplyr::filter(
            years_to_death < 5 & event_death == 0
          ) %>%
          dplyr::select(
            `.id`, differentiation, positive_nodes
          )
      ),
    by = ".id"
  ) %>%
  dplyr::select(
    dplyr::all_of(x = names(colon_cancer))
  ) %>%
  as.data.frame()

usethis::use_data(colon_cancer, overwrite = TRUE)

# Subset colon cancer data to active treatment arms: Lev, Lev+5FU
colon_cancer_active <-
  subset(
    x = colon_cancer,
    subset = arm %in% c("Lev+5FU", "Lev")
  )

colon_cancer_active$tx <-
  with(
    data = colon_cancer_active,
    1*(arm == "Lev+5FU") + 0*(arm == "Lev")
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

usethis::use_data(colon_cancer_active, overwrite = TRUE)
