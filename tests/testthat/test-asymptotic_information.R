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
