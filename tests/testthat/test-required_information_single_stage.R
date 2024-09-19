test_that(
  desc = "Error Handling Works",
  code = {
    expect_error(
      object =
        required_information_single_stage(
          delta = c(5, 7.5),
          delta_0 = 0,
          alpha = 0.05,
          sides = 0.5,
          power = c(0.8, 0.9)
        ),
      regexp = "`sides`"
    )

    expect_error(
      object =
        required_information_single_stage(
          delta = c(5, 7.5),
          delta_0 = 0,
          alpha = 0.05,
          sides = 3,
          power = c(0.8, 0.9)
        ),
      regexp = "`sides`"
    )

    expect_error(
      object =
        required_information_single_stage(
          delta = c(5, 7.5),
          delta_0 = 0,
          alpha = 0.05,
          sides = 2,
          power = c(0.8, 0.9, 1)
        ),
      regexp = "`power`"
    )

    expect_error(
      object =
        required_information_single_stage(
          delta = c(5, 7.5),
          delta_0 = 0,
          alpha = 0.05,
          sides = 2,
          power = c(0.025, 0.9)
        ),
      regexp = "`power`"
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        required_information_single_stage(
          delta = c(5),
          delta_0 = 0,
          alpha = 0.05,
          sides = 2,
          power = c(0.8)
        )
    )

    expect_no_condition(
      object =
        # Grid of Values
        required_information_single_stage(
          delta = c(5, 7.5, 10),
          delta_0 = 0,
          alpha = 0.05,
          sides = 2,
          power = c(0.8, 0.9)
        )
    )

  }
)
