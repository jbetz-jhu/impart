test_that(
  desc = "Complete Data",
  code =
    {
      colon_cancer_5fu_vs_obs <-
        subset(
          x = as.data.frame(colon_cancer),
          arm %in% c("Lev+5FU", "Obs")
        ) |>
        droplevels()

      expect_no_condition(
        object =
          speffsurv_impart(
            data = colon_cancer_5fu_vs_obs,
            estimand = "log_hazard_ratio",
            formula =
              survival::Surv(time = years_to_death, event = event_death) ~
              age + sex + obstruction + perforation + organ_adherence +
              positive_nodes + differentiation + local_spread,
            treatment_column = "arm",
            alpha = 0.05,
            ci = FALSE
          )
      )
    }
)




test_that(
  desc = "Missing Covariates",
  code =
    {
      set.seed(seed = 12345)
      colon_cancer_5fu_vs_obs_mcar <-
        subset(
          x = as.data.frame(colon_cancer),
          arm %in% c("Lev+5FU", "Obs")
        ) |>
        droplevels()
      miss_rows <- sample(x = 1:nrow(colon_cancer_5fu_vs_obs_mcar), size = 10)
      colon_cancer_5fu_vs_obs_mcar$age[miss_rows] <- NA
      colon_cancer_5fu_vs_obs_mcar$sex[miss_rows] <- NA
      colon_cancer_5fu_vs_obs_mcar$positive_nodes[miss_rows] <- NA

      expect_no_condition(
        object =
          speffsurv_impart(
            data = colon_cancer_5fu_vs_obs_mcar,
            estimand = "log_hazard_ratio",
            formula =
              survival::Surv(time = years_to_death, event = event_death) ~
              age + sex + obstruction + perforation + organ_adherence +
              positive_nodes + differentiation + local_spread,
            treatment_column = "arm",
            alpha = 0.05,
            ci = TRUE
          )
      )
    }
)




test_that(
  desc = "Missing Outcome Indicator",
  code =
    {
      set.seed(seed = 23456)
      colon_cancer_5fu_vs_obs_mcar <-
        subset(
          x = as.data.frame(colon_cancer),
          arm %in% c("Lev+5FU", "Obs")
        ) |>
        droplevels()
      miss_rows <- sample(x = 1:nrow(colon_cancer_5fu_vs_obs_mcar), size = 10)
      colon_cancer_5fu_vs_obs_mcar$event_death[miss_rows] <- NA

      expect_error(
        object =
          speffsurv_impart(
            data = colon_cancer_5fu_vs_obs_mcar,
            estimand = "log_hazard_ratio",
            formula =
              survival::Surv(time = years_to_death, event = event_death) ~
              age + sex + obstruction + perforation + organ_adherence +
              positive_nodes + differentiation + local_spread,
            treatment_column = "arm",
            alpha = 0.05,
            ci = TRUE
          ),
        regexp = "Indicators missing an event time"
      )
    }
)




test_that(
  desc = "Missing Outcome Times",
  code =
    {
      set.seed(seed = 23456)
      colon_cancer_5fu_vs_obs_mcar <-
        subset(
          x = as.data.frame(colon_cancer),
          arm %in% c("Lev+5FU", "Obs")
        ) |>
        droplevels()
      miss_rows <- sample(x = 1:nrow(colon_cancer_5fu_vs_obs_mcar), size = 10)
      colon_cancer_5fu_vs_obs_mcar$years_to_death[miss_rows] <- NA

      expect_error(
        object =
          speffsurv_impart(
            data = colon_cancer_5fu_vs_obs_mcar,
            estimand = "log_hazard_ratio",
            formula =
              survival::Surv(time = years_to_death, event = event_death) ~
              age + sex + obstruction + perforation + organ_adherence +
              positive_nodes + differentiation + local_spread,
            treatment_column = "arm",
            alpha = 0.05,
            ci = TRUE
          ),
        regexp = "Indicators missing an event time"
      )
    }
)
