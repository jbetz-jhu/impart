
set.seed(12345)
test_data <-
  data.frame(
    id = 1:100,
    y = rnorm(n = 100),
    group = ceiling(1:100/25)
  )

get_lm_intercept <-
  function(
    data, formula
  ){
    lm_fit <- lm(formula = formula, data = data)

    return(c(estimate = as.numeric(coef(lm_fit)["(Intercept)"])))
  }

# 1. Input Error Handling ######################################################
testthat::test_that(
  desc = "1. Input Error Handling",
  code = {
    ## 1.1 Data argument not correctly specified #########################
    testthat::expect_error(
      object =
        with(
          calculate_estimate(
            data = "test_data",
            estimation_function = get_lm_intercept,
            estimation_arguments =
              list(
                formula = y ~ 1
              )
            )
        ),
      regexp = "must be a data.frame"
    )
  }
)
