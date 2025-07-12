test_that(
  desc = "Error Handling Works",
  code = {
    expect_error(
      object =
        asymptotic_information_logrank(
          allocation_ratio = NA,
          total_events = c(66, 90)
        ),
      regex = "All elements of `allocation_ratio` must be numeric"
    )

    expect_error(
      object =
        asymptotic_information_logrank(
          allocation_ratio = 0,
          total_events = c(66, 90)
        ),
      regex = "All elements of `allocation_ratio` must be greater than 0."
    )

    expect_error(
      object =
        asymptotic_information_logrank(
          allocation_ratio = 1,
          total_events = -2
        ),
      regex = "`total_events` must be greater than 1."
    )
  }
)


test_that(
  desc = "Works with Valid Input",
  code = {
    expect_no_condition(
      object =
        asymptotic_information_logrank(
          allocation_ratio = 1,
          total_events = 20
        )
    )

    expect_no_condition(
      object =
        asymptotic_information_logrank(
          allocation_ratio = 1,
          total_events = c(66, 90)
        )
    )
  }
)
