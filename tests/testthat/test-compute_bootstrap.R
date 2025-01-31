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


bootstrap_result <-
  compute_bootstrap_serial(
    data = test_data,
    ids = cbind(rep(1, 100), rep(2, 100), rep(3, 100)),
    estimation_function = get_lm_intercept,
    estimation_arguments =
      list(
        formula = y ~ 1
      )
  )

bootstrap_results_parallel <-
  compute_bootstrap_parallel(
    data = test_data,
    ids = cbind(rep(1, 100), rep(2, 100), rep(3, 100)),
    estimation_function = get_lm_intercept,
    estimation_arguments =
      list(
        formula = y ~ 1
      )
  )

test_that(
  desc = "Bootstrap Works",
  code = {
    expect_equal(
      object = bootstrap_result[1],
      expected = test_data$y[1]
    )

    expect_equal(
      object = bootstrap_result[2],
      expected = test_data$y[2]
    )

    expect_equal(
      object = bootstrap_result[3],
      expected = test_data$y[3]
    )


    expect_equal(
      object = bootstrap_results_parallel[1],
      expected = test_data$y[1]
    )

    expect_equal(
      object = bootstrap_results_parallel[2],
      expected = test_data$y[2]
    )

    expect_equal(
      object = bootstrap_results_parallel[3],
      expected = test_data$y[3]
    )
  }
)
