test_that(
  desc = "Error Handling Works",
  code = {
    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          mw =
            mw_from_pmfs(
              pmf_1 = c(0.5, 0.25, 0.25),
              pmf_0 = c(0.6, 0.2, 0.2)
            ),
          pmf_1 = c(0.5, 0.25, 0.25),
          pmf_0 = c(0.6, 0.2, 0.2),
          adjust = TRUE
        ),
      regex = "or `mw` should be specified"
    )

    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = -1,
          n_1 = 100,
          mw =
            mw_from_pmfs(
              pmf_1 = c(0.5, 0.25, 0.25),
              pmf_0 = c(0.6, 0.2, 0.2)
            )
        ),
      regex = "All elements of `n_0` and `n_1`"
    )

    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          mw =
            mw_from_pmfs(
              pmf_1 = c(0.5, 0.25, 0.25),
              pmf_0 = c(0.6, 0.2, 0.2)
            ),
          adjust = TRUE
        ),
      regex = "Adjustment for ties"
    )

    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          pmf_1 = c(0.5, 0.25, 0.25),
          pmf_0 = c(0.6, 0.2, 0.25)
        ),
      regex = "`pmf_0` does not sum to 1"
    )

    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          pmf_1 = c(0.5, 0.25, 0.2),
          pmf_0 = c(0.6, 0.2, 0.2)
        ),
      regex = "`pmf_1` does not sum to 1"
    )

    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          pmf_0 =
            rbind(
              c(0.5, 0.25, 0.2),
              c(0.5, 0.3, 0.2)
            ),
          pmf_1 =
            rbind(
              c(0.6, 0.2, 0.2),
              c(0.6, 0.3, 0.1)
            ),
          adjust = TRUE
        ),
      regex = "Each row of `pmf_0` should sum to 1"
    )

    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          pmf_0 =
            rbind(
              c(0.5, 0.25, 0.25),
              c(0.5, 0.3, 0.2)
            ),
          pmf_1 =
            rbind(
              c(0.6, 0.2, 0.4),
              c(0.6, 0.3, 0.1)
            ),
          adjust = TRUE
        ),
      regex = "Each row of `pmf_1` should sum to 1"
    )

    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          mw = NULL,
          pmf_1 = NULL,
          pmf_0 = NULL,
          adjust = FALSE
        ),
      regex = "If `mw` is NULL then `pmf_1` and `pmf_0`"
    )

    expect_error(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          pmf_1 = c(0.5, 0.25, 0.25),
          pmf_0 = c(0.6, 0.4)
        ),
      regex = "Number of levels in `pmf_0`"
    )
  }
)

test_that(
  desc = "Valid Inputs Give Output",
  code = {
    # Single Parameter Set - PMFs
    expect_no_condition(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          pmf_1 = c(0.5, 0.25, 0.25),
          pmf_0 = c(0.1, 0.1, 0.8)
        )
    )

    # Single Parameter Set - MW
    expect_no_condition(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          mw =
            mw_from_pmfs(
              pmf_1 = c(0.5, 0.25, 0.25),
              pmf_0 = c(0.1, 0.1, 0.8)
            ),
          adjust = FALSE
        )
    )

    # Grid of Values - PMFs
    expect_no_condition(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          pmf_1 =
            rbind(
              c(0.5, 0.25, 0.25),
              c(0.6, 0.2, 0.2)
            ),
          pmf_0 =
            rbind(
              c(0.1, 0.1, 0.8),
              c(0.05, 0.05, 0.9)
            )
        )
    )

    # Grid of Values - MW
    expect_no_condition(
      object =
        asymptotic_information_mann_whitney_fm(
          n_0 = 100,
          n_1 = 100,
          mw = c(0.75, 0.8),
          adjust = FALSE
        )
    )
  }
)
