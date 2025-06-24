test_that(
  desc = "Works with Valid Input",
  code =
    {
      set.seed(seed = 12345)
      n_obs <- nrow(sim_colon_cancer)
      n_missing <- 10
      sim_colon_cancer_x_mcar <- sim_colon_cancer
      sim_colon_cancer_x_mcar$age[sample(x = 1:n_obs, size = n_missing)] <- NA
      sim_colon_cancer_x_mcar$sex[sample(x = 1:n_obs, size = n_missing)] <- NA
      sim_colon_cancer_x_mcar$positive_nodes[
        sample(x = 1:n_obs, size = n_missing)
      ] <- NA

      # Complete Data
      expect_no_condition(
        object =
          speffsurv_impart(
            data = sim_colon_cancer,
            estimand = "log_hazard_ratio",
            formula =
              survival::Surv(time = years_to_death, event = event_death) ~
              age + sex + obstruction + perforation + organ_adherence +
              positive_nodes + differentiation + local_spread,
            treatment_column = "tx",
            alpha = 0.05,
            ci = FALSE
          )
      )

      # Missing Covariates
      expect_no_condition(
        object =
          speffsurv_impart(
            data = sim_colon_cancer_x_mcar,
            estimand = "log_hazard_ratio",
            formula =
              survival::Surv(time = years_to_death, event = event_death) ~
              age + sex + obstruction + perforation + organ_adherence +
              positive_nodes + differentiation + local_spread,
            treatment_column = "tx",
            alpha = 0.05,
            ci = TRUE
          )
      )
    }
)




test_that(
  desc = "Error Handling Works",
  code =
    {
      set.seed(seed = 23456)
      miss_rows <- sample(x = 1:nrow(sim_colon_cancer), size = 10)
      sim_colon_cancer_miss_event <-
        sim_colon_cancer_miss_time <-
        sim_colon_cancer
      sim_colon_cancer_miss_event$event_death[miss_rows] <- NA
      sim_colon_cancer_miss_time$years_to_death[miss_rows] <- NA


      # Missing Event Indicator
      expect_error(
        object =
          speffsurv_impart(
            data = sim_colon_cancer_miss_event,
            estimand = "log_hazard_ratio",
            formula =
              survival::Surv(time = years_to_death, event = event_death) ~
              age + sex + obstruction + perforation + organ_adherence +
              positive_nodes + differentiation + local_spread,
            treatment_column = "tx",
            alpha = 0.05,
            ci = TRUE
          ),
        regexp = "Indicators missing an event time"
      )

      expect_error(
        object =
          speffsurv_impart(
            data = sim_colon_cancer_miss_time,
            estimand = "log_hazard_ratio",
            formula =
              survival::Surv(time = years_to_death, event = event_death) ~
              age + sex + obstruction + perforation + organ_adherence +
              positive_nodes + differentiation + local_spread,
            treatment_column = "tx",
            alpha = 0.05,
            ci = TRUE
          ),
        regexp = "Indicators missing an event time"
      )
    }
)
