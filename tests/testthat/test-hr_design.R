test_that(
  desc = "Error Handling Works",
  code = {

    ### test_sides #############################################################
    expect_error(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = NULL,
          ratio = 1
        )
      },
      regex = "`test_sides` must be equal to `1` or `2`"
    )

    expect_error(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = NA,
          ratio = 1
        )
      },
      regex = "`test_sides` must be equal to `1` or `2`"
    )

    expect_error(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = 3,
          ratio = 1
        )
      },
      regex = "`test_sides` must be equal to `1` or `2`"
    )

    expect_error(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = c(1, 2, 3),
          ratio = 1
        )
      },
      regex = "`test_sides` must be equal to `1` or `2`"
    )

    ### ratio ##################################################################
    expect_error(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = 2,
          ratio = NULL
        )
      },
      regex = "`ratio` must be numeric and positive"
    )

    expect_error(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = 2,
          ratio = NA
        )
      },
      regex = "`ratio` must be numeric and positive"
    )

    expect_error(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = 2,
          ratio = -2
        )
      },
      regex = "`ratio` must be numeric and positive"
    )

    expect_error(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = 2,
          ratio = c(1, -2)
        )
      },
      regex = "`ratio` must be numeric and positive"
    )

    ### hazard_ratio ###########################################################
    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NA,
          power = 0.80,
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `hazard_ratio` must be finite and not equal to 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = c(2, NA),
          power = 0.80,
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `hazard_ratio` must be finite and not equal to 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = 1,
          power = 0.80,
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `hazard_ratio` must be finite and not equal to 1"
    )

    ### power ##################################################################
    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = NA,
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `power` must be > 0 and < 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = c(-1),
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `power` must be > 0 and < 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = c(-1, 0.8),
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `power` must be > 0 and < 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = c(1, 0.8),
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `power` must be > 0 and < 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = c(NA, 0.8),
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `power` must be > 0 and < 1"
    )

    ### alpha ##################################################################
    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = 0.8,
          alpha = NA,
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `alpha` must be > 0 and < 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = 0.8,
          alpha = c(-1),
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `alpha` must be > 0 and < 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = 0.80,
          alpha = c(-1, 0.05),
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `alpha` must be > 0 and < 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = 0.80,
          alpha = c(1, 0.05),
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `alpha` must be > 0 and < 1"
    )

    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = 0.80,
          alpha = c(NA, 0.05),
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "All elements of `alpha` must be > 0 and < 1"
    )

    ### all arguments ##########################################################
    expect_error(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = 0.8,
          power = 0.80,
          alpha = c(0.05),
          test_sides = 2,
          ratio = 1
        )
      },
      regex = "Exactly one of `events`, `hazard_ratio`, `power`, and `alpha`"
    )
  }
)


test_that(
  desc = "Works with Valid Input",
  code = {
    ### Events #################################################################
    # Scalar Inputs
    expect_no_condition(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = 0.5,
          power = 0.8,
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      }
    )

    # Vector Inputs
    expect_no_condition(
      object = {
        hr_design(
          events = NULL,
          hazard_ratio = c(0.5, 1/0.5),
          power = c(0.8, 0.9),
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      }
    )

    ### Hazard Ratio ###########################################################
    # Scalar Inputs
    expect_no_condition(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = 0.8,
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      }
    )

    # Vector Inputs
    expect_no_condition(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = NULL,
          power = c(0.8, 0.9),
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      }
    )

    ### Power ##################################################################
    # Scalar Inputs
    expect_no_condition(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = 0.5,
          power = NULL,
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      }
    )

    # Vector Inputs
    expect_no_condition(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = c(0.5, 0.6),
          power = NULL,
          alpha = 0.05,
          test_sides = 2,
          ratio = 1
        )
      }
    )


    ### Alpha ##################################################################
    # Scalar Inputs
    expect_no_condition(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = 0.5,
          power = 0.80,
          alpha = NULL,
          test_sides = 2,
          ratio = 1
        )
      }
    )

    # Vector Inputs
    expect_no_condition(
      object = {
        hr_design(
          events = 100,
          hazard_ratio = 0.5,
          power = c(0.80, 0.90),
          alpha = NULL,
          test_sides = 2,
          ratio = 1
        )
      }
    )
  }
)


test_that(
  desc = "Check Inverses",
  code = {
    hazard_ratio <- 0.75
    power <- 0.8
    alpha <- 0.05
    test_sides <- 2
    ratio <- 1

    expect_equal(
      object =
        hr_power(
          events =
            hr_events(hazard_ratio = hazard_ratio, power = power, alpha = alpha,
                      test_sides = test_sides, ratio = ratio),
          hazard_ratio = hazard_ratio,
          alpha = alpha,
          test_sides = test_sides,
          ratio = ratio
        ),
      expected = power,
      ignore_attr = TRUE
    )

    expect_equal(
      object =
        hr_alpha(
          events =
            hr_events(hazard_ratio = hazard_ratio, power = power, alpha = alpha,
                      test_sides = test_sides, ratio = ratio),
          hazard_ratio = hazard_ratio,
          power = power,
          test_sides = test_sides,
          ratio = ratio
        ),
      expected = alpha,
      ignore_attr = TRUE
    )

    expect_equal(
      object =
        hr_minimal(
          events =
            hr_events(hazard_ratio = hazard_ratio, power = power, alpha = alpha,
                      test_sides = test_sides, ratio = ratio),
          power = power,
          alpha = alpha,
          test_sides = test_sides,
          ratio = ratio
        )$hazard_ratio |> as.numeric(),
      expected = hazard_ratio,
      ignore_attr = TRUE
    )

  }
)
