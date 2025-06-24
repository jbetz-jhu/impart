test_that(
  desc = "Error Handling Works - Difference In Means",
  code = {
    expect_error(
      object =
        asymptotic_information_difference_means(
          n_1 = c(100),
          n_0 = c(-1),
          sigma_0 = 1,
          sigma_1 = 1
        ),
      regexp = "All elements of `n_0` and `n_1` must be greater than 1"
    )

    expect_error(
      object =
        asymptotic_information_difference_means(
          n_1 = c(-1),
          n_0 = c(100),
          sigma_0 = 1,
          sigma_1 = 1
        ),
      regexp = "All elements of `n_0` and `n_1` must be greater than 1"
    )

    expect_error(
      object =
        asymptotic_information_difference_means(
          n_1 = c(100, 150),
          n_0 = c(100, 150),
          sigma_0 = c(0, 1),
          sigma_1 = c(1)
        ),
      regexp = "All elements of `sigma_0` and `sigma_1` must be positive"
    )

    expect_error(
      object =
        asymptotic_information_difference_means(
          n_1 = c(100, 150),
          n_0 = c(100, 150),
          sigma_0 = c(1),
          sigma_1 = c(0, 1)
        ),
      regexp = "All elements of `sigma_0` and `sigma_1` must be positive"
    )
  }
)


test_that(
  desc = "Valid Inputs Give Output",
  code = {
    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_means(
          n_1 = 100,
          n_0 = 100,
          sigma_0 = 1,
          sigma_1 = 1
          )
      )

    # Grid of Values
    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_means(
          n_1 = c(100, 150),
          n_0 = 100,
          sigma_0 = 1,
          sigma_1 = 1
        )
    )

    # Grid of Values
    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_means(
          n_1 = 100,
          n_0 = c(100, 150),
          sigma_0 = 1,
          sigma_1 = 1
        )
    )

    # Grid of Values
    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_means(
          n_1 = 100,
          n_0 = 100,
          sigma_0 = c(1, 2),
          sigma_1 = 1
        )
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_means(
          n_1 = 100,
          n_0 = 100,
          sigma_0 = 1,
          sigma_1 = c(1, 2)
        )
    )
  }
)




test_that(
  desc = "Error Handling Works - Difference In Proportions",
  code = {
    expect_error(
      object =
        asymptotic_information_difference_proportions(
          n_1 = c(100),
          n_0 = c(-1),
          pi_0 = 0.5,
          pi_1 = 0.75
        ),
      regexp = "All elements of `n_0` and `n_1` must be greater than 1"
    )

    expect_error(
      object =
        asymptotic_information_difference_proportions(
          n_1 = c(-1),
          n_0 = c(100),
          pi_0 = 0.5,
          pi_1 = 0.75
        ),
      regexp = "All elements of `n_0` and `n_1` must be greater than 1"
    )

    expect_error(
      object =
        asymptotic_information_difference_proportions(
          n_1 = c(100, 150),
          n_0 = c(100, 150),
          pi_0 = -0.5,
          pi_1 = 0.75
        ),
      regexp = "All elements of `pi_0` and `pi_1` must be positive"
    )

    expect_error(
      object =
        asymptotic_information_difference_proportions(
          n_1 = c(100, 150),
          n_0 = c(100, 150),
          pi_0 = 0.5,
          pi_1 = -0.75
        ),
      regexp = "All elements of `pi_0` and `pi_1` must be positive"
    )

    expect_error(
      object =
        asymptotic_information_difference_proportions(
          n_1 = c(100, 150),
          n_0 = c(100, 150),
          pi_0 = 1.5,
          pi_1 = 0.75
        ),
      regexp = "All elements of `pi_0` and `pi_1` must be less than 1"
    )

    expect_error(
      object =
        asymptotic_information_difference_proportions(
          n_1 = c(100, 150),
          n_0 = c(100, 150),
          pi_0 = 0.5,
          pi_1 = 1.75
        ),
      regexp = "All elements of `pi_0` and `pi_1` must be less than 1"
    )
  }
)




test_that(
  desc = "Valid Inputs Give Output",
  code = {
    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_proportions(
          n_1 = 100,
          n_0 = 100,
          pi_0 = 0.5,
          pi_1 = 0.75
        )
    )

    # Grid of Values
    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_proportions(
          n_1 = c(100, 150),
          n_0 = 100,
          pi_0 = 0.5,
          pi_1 = 0.75
        )
    )

    # Grid of Values
    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_proportions(
          n_1 = 100,
          n_0 = c(100, 150),
          pi_0 = 0.5,
          pi_1 = 0.75
        )
    )

    # Grid of Values
    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_proportions(
          n_1 = 100,
          n_0 = 100,
          pi_0 = c(0.5, 0.6),
          pi_1 = 0.75
        )
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        asymptotic_information_difference_proportions(
          n_1 = 100,
          n_0 = 100,
          pi_0 = 0.5,
          pi_1 = c(0.75, 0.80)
        )
    )
  }
)

test_that(
  desc = "DIM Information Matches Sample Size Calculations",
  code = {
    mcid <- 5
    power <- 0.80
    alpha <- 0.05
    test_sides <- 2

    sd <- 10

    n_per_arm <-
      power.t.test(
        d = mcid/sd,
        sig.level = alpha,
        power = power,
        type = "two.sample"
      )$n

    # Expect some difference: t-test vs. z-test
    expect_lt(
      object =
        abs(
          impart::required_information_single_stage(
            delta = mcid,
            alpha = alpha,
            power = power
          ) -
            asymptotic_information_difference_means(
              n_0 = n_per_arm,
              n_1 = n_per_arm,
              sigma_0 = sd,
              sigma_1 = sd
            )
        ),
      expected = 1e-2
    )
  }
)


test_that(
  desc = "RD Information Matches Sample Size Calculations",
  code = {
    pi_0 <- 0.5
    pi_1 <- 0.25
    power <- 0.80
    alpha <- 0.05
    test_sides <- 2

    n_per_arm <-
      power.prop.test(
        p1 = pi_1,
        p2 = pi_0,
        sig.level = alpha,
        power = power,
        alternative = "two.sided"
      )$n

    # Expect some difference: Arcsin transform vs. Asymptotic Normal
    expect_lt(
      object =
        abs(
          1 - impart::required_information_single_stage(
            delta = pi_1 - pi_0,
            alpha = alpha,
            power = power
          )/asymptotic_information_difference_proportions(
            n_0 = n_per_arm,
            n_1 = n_per_arm,
            pi_1 = pi_1,
            pi_0 = pi_0
          )
        ),
      expected = 0.05
    )
  }
)


test_that(
  desc = "RR Information Matches Sample Size Calculations",
  code = {
    pi_0 <- 0.5
    pi_1 <- 0.25
    power <- 0.80
    alpha <- 0.05
    test_sides <- 2

    n_per_arm <-
      rr_design(
        pi_1 = pi_1,
        pi_0 = pi_0,
        power = power,
        alpha = alpha,
        test_sides = test_sides
      )$n_per_arm

    expect_lt(
      object =
        abs(
          impart::required_information_single_stage(
            delta = log(pi_1/pi_0),
            alpha = alpha,
            power = power
          ) -
            asymptotic_information_relative_risk(
              n_0 = n_per_arm,
              n_1 = n_per_arm,
              pi_1 = pi_1,
              pi_0 = pi_0
            )
        ),
      expected = 1e-8
    )
  }
)
