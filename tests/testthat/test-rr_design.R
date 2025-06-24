test_that(
  desc = "Error Handling Works",
  code = {
    ### test_sides #############################################################
    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = NULL
        )
      },
      regex = "`test_sides` must be equal to `1` or `2`"
    )

    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = NA
        )
      },
      regex = "`test_sides` must be equal to `1` or `2`"
    )

    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = 3
        )
      },
      regex = "`test_sides` must be equal to `1` or `2`"
    )

    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = c(1, 2, 3)
        )
      },
      regex = "`test_sides` must be equal to `1` or `2`"
    )

    ### power ##################################################################
    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          power = -1,
          alpha = 0.05,
          test_sides = c(1, 2)
        )
      },
      regex = "All elements of `power` must be > 0 and < 1"
    )

    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          power = c(0.80, 1.5),
          alpha = 0.05,
          test_sides = c(1, 2)
        )
      },
      regex = "All elements of `power` must be > 0 and < 1"
    )

    ### alpha ##################################################################
    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          power = 0.80,
          alpha = -1,
          test_sides = c(1, 2)
        )
      },
      regex = "All elements of `alpha` must be > 0 and < 1"
    )

    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          power = 0.80,
          alpha = c(0.05, 3),
          test_sides = c(1, 2)
        )
      },
      regex = "All elements of `alpha` must be > 0 and < 1"
    )

    ### n_per_arm ##############################################################
    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          n_per_arm = NA,
          power = 0.80,
          alpha = 0.05,
          test_sides = 1
        )
      },
      regex = "All elements of `n_per_arm` must be >= 1"
    )

    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          n_per_arm = 0,
          power = 0.80,
          alpha = 0.05,
          test_sides = 1
        )
      },
      regex = "All elements of `n_per_arm` must be >= 1"
    )

    expect_error(
      object = {
        rr_design(
          pi_1 = NULL,
          pi_0 = 0.5,
          rr = 0.5,
          power = NULL,
          alpha = 0.05,
          test_sides = 1
        )
      },
      regex = "If risk is specified in each arm"
    )

    expect_error(
      object = {
        rr_design(
          pi_1 = NULL,
          pi_0 = 0.5,
          rr = 0.5,
          power = 0.80,
          alpha = NULL,
          test_sides = 1
        )
      },
      regex = "If risk is specified in each arm"
    )

    expect_error(
      object = {
        rr_design(
          n_per_arm = 51,
          pi_1 = NULL,
          pi_0 = 0.5,
          rr = 0.5,
          power = NULL,
          alpha = NULL,
          test_sides = 1
        )
      },
      regex = "If risk is specified in each arm"
    )

    ### specify pi_1, pi_0, rr #################################################
    expect_error(
      object = {
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          rr = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = 1
        )
      },
      regex = "At least one of `pi_1`, `pi_0`, or `rr` must be NULL"
    )
  }
)

test_that(
  desc = "Works With Valid Input",
  code = {
    ### n_per_arm ##############################################################
    expect_identical(
      object =
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          rr = NULL,
          power = 0.80,
          alpha = 0.05,
          test_sides = 1
        ),
      expected =
        rr_design(
          pi_1 = 0.25,
          pi_0 = NULL,
          rr = 0.5,
          power = 0.80,
          alpha = 0.05,
          test_sides = 1
        )
    )

    expect_identical(
      object =
        rr_design(
          pi_1 = 0.25,
          pi_0 = 0.5,
          rr = NULL,
          power = c(0.80, 0.90),
          alpha = 0.05,
          test_sides = 1
        ),
      expected =
        rr_design(
          pi_1 = NULL,
          pi_0 = 0.5,
          rr = 0.5,
          power = c(0.80, 0.90),
          alpha = 0.05,
          test_sides = 1
        )
    )

    ### power ##################################################################
    expect_identical(
      object =
        rr_design(
          n_per_arm = 52,
          pi_1 = 0.25,
          pi_0 = 0.5,
          rr = NULL,
          alpha = 0.05,
          test_sides = 1
        ),
      expected =
        rr_design(
          n_per_arm = 52,
          pi_1 = 0.25,
          pi_0 = NULL,
          rr = 0.5,
          alpha = 0.05,
          test_sides = 1
        )
    )

    expect_identical(
      object =
        rr_design(
          n_per_arm = c(52, 72),
          pi_1 = 0.25,
          pi_0 = 0.5,
          rr = NULL,
          alpha = 0.05,
          test_sides = 1
        ),
      expected =
        rr_design(
          n_per_arm = c(52, 72),
          pi_1 = 0.25,
          pi_0 = NULL,
          rr = 0.5,
          alpha = 0.05,
          test_sides = 1
        )
    )

    ### alpha ##################################################################
    expect_identical(
      object =
        rr_design(
          n_per_arm = 52,
          pi_1 = 0.25,
          pi_0 = 0.5,
          rr = NULL,
          power = 0.80,
          test_sides = 1
        ),
      expected =
        rr_design(
          n_per_arm = 52,
          pi_1 = 0.25,
          pi_0 = NULL,
          rr = 0.5,
          power = 0.80,
          test_sides = 1
        )
    )

    expect_identical(
      object =
        rr_design(
          n_per_arm = c(52, 72),
          pi_1 = 0.25,
          pi_0 = 0.5,
          rr = NULL,
          power = 0.80,
          test_sides = 1
        ),
      expected =
        rr_design(
          n_per_arm = c(52, 72),
          pi_1 = 0.25,
          pi_0 = NULL,
          rr = 0.5,
          power = 0.80,
          test_sides = 1
        )
    )

    ### relative risk ##########################################################
    expect_no_condition(
      object =
        rr_design(
          n_per_arm = 200,
          pi_0 = 0.5,
          rr = NULL,
          rr_above_1 = FALSE,
          power = 0.80,
          alpha = 0.05,
          test_sides = 2
        )
    )

    expect_no_condition(
      object =
        rr_design(
          n_per_arm = c(200, 250),
          pi_0 = c(0.5, 0.45),
          rr = NULL,
          rr_above_1 = FALSE,
          power = 0.80,
          alpha = 0.05,
          test_sides = 2
        )
    )
  }
)


test_that(
  desc = "Check Inverses",
  code = {
    pi_0 <- 0.08
    rr <- 0.8
    power <- 0.8
    alpha <- 0.05
    test_sides <- 2

    expect_equal(
      object =
        rr_power(
          n_per_arm =
            rr_n_per_arm(
              pi_0 = pi_0,
              rr = rr,
              power = power,
              alpha = alpha,
              test_sides = test_sides
            )$n_per_arm,
          pi_0 = pi_0,
          rr = rr,
          alpha = alpha,
          test_sides = test_sides
        ),
      expected = power,
      ignore_attr = TRUE
    )

    expect_equal(
      object =
        rr_alpha(
          n_per_arm =
            rr_n_per_arm(
              pi_0 = pi_0,
              rr = rr,
              power = power,
              alpha = alpha,
              test_sides = test_sides
            )$n_per_arm,
          pi_0 = pi_0,
          rr = rr,
          power = power,
          test_sides = test_sides
        ),
      expected = alpha,
      ignore_attr = TRUE
    )

    expect_equal(
      object =
        rr_minimal(
          n_per_arm =
            rr_n_per_arm(
              pi_0 = pi_0,
              rr = rr,
              power = power,
              alpha = alpha,
              test_sides = test_sides
            )$n_per_arm,
          pi_0 = pi_0,
          rr_above_1 = FALSE,
          power = power,
          alpha = alpha,
          test_sides = test_sides
        ),
      expected = rr,
      ignore_attr = TRUE
    )
  }
)
