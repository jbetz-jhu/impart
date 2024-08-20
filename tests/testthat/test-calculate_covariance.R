set.seed(12345)

test_data <-
  data.frame(
    .id = 1:100,
    y = rnorm(n = 100),
    group = ceiling(1:100/25)
  )

get_lm_intercept <-
  function(
    data, formula
  ){
    intercept <- coef(lm(formula = formula, data = data))["(Intercept)"]
    return(c("estimate" = as.numeric(intercept)))
  }


covariance_1_result <-
  tryCatch(
    expr = {
      calculate_covariance(
        data = subset(x = test_data, group %in% 1),
        ids_by_analysis = NULL,
        bootstrap_ids = NULL,
        bootstrap_results = NULL,
        estimation_function = get_lm_intercept,
        estimation_arguments =
          list(
            formula = y ~ 1
          ),
        rng_seed = 23456,
        control = monitored_analysis_control_testing()
      )
    },
    error = {"error"},
    warning = {"warning"},
    finally = {}
  )

covariance_2_result <-
  tryCatch(
    expr = {
      calculate_covariance(
        data = subset(x = test_data, group %in% 1:2),
        ids_by_analysis = covariance_1_result$ids_by_analysis,
        bootstrap_ids = covariance_1_result$bootstrap_ids,
        bootstrap_results = covariance_1_result$bootstrap_results,
        estimation_function = get_lm_intercept,
        estimation_arguments =
          list(
            formula = y ~ 1
          ),
        rng_seed = 23456,
        control = monitored_analysis_control_testing()
      )
    },
    error = {"error"},
    warning = {"warning"},
    finally = {}
  )


test_that(
  desc = "Two-Stage Covariance",
  code = {
    expect_contains(
      object = names(covariance_1_result),
      expected = c("covariance", "ids_by_analysis", "bootstrap_estimates",
                   "bootstrap_ids", "random_seed")
    )

    expect_contains(
      object = names(covariance_2_result),
      expected = c("covariance", "ids_by_analysis", "bootstrap_estimates",
                   "bootstrap_ids", "random_seed")
    )
  }
)
