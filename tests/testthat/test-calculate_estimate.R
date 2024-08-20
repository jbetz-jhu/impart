set.seed(12345)

test_data <-
  data.frame(
    .id = 1:100,
    y = rnorm(n = 100),
    group = ceiling(1:100/25)
  )

# Note: Result contains element named "estimate"
get_lm_intercept <-
  function(
    data, formula
  ){
    intercept <- coef(lm(formula = formula, data = data))["(Intercept)"]
    return(c("estimate" = as.numeric(intercept)))
  }

# Note: Result named 'eestimate' instead of 'estimate'
get_lm_intercept_error <-
  function(
    data, formula
  ){
    intercept <- coef(lm(formula = formula, data = data))["(Intercept)"]
    return(c("eestimate" = as.numeric(intercept)))
  }

test_that(
  desc = "Error Handling",
  code = {
    expect_no_condition(
      object =
        calculate_estimate(
          data = test_data,
          estimation_function = get_lm_intercept,
          estimation_arguments =
            list(
              formula = y ~ 1
            )
        )
    )

    expect_error(
      object =
        calculate_estimate(
          data = test_data,
          estimation_function = get_lm_intercept_error,
          estimation_arguments =
            list(
              formula = y ~ 1
            )
        ),
      regexp = "element named 'estimate'"
    )
  }
)


example_1$.r_4 <- 1*(!is.na(example_1$y_4))

test_that(
  desc = "Standardization Works",
  code = {
    expect_no_condition(
      object =
        calculate_estimate(
          data = example_1,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            )
        )
    )
  }
)
