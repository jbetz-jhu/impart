test_that(
  desc = "Error Handling Works - Continuous",
  code = {
    expect_error(
      object =
        information_to_n_difference_means(
          information = -1,
          sigma_0 = c(1, 2),
          sigma_1 = c(1, 2)
        ),
      regexp = "`information` must be positive"
    )

    expect_error(
      object =
        information_to_n_difference_means(
          information = 5,
          sigma_0 = c(0, 2),
          sigma_1 = c(-1, 2)
        ),
      regexp = "`sigma_0` and `sigma_1` must be positive"
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        information_to_n_difference_means(
          information = 5,
          sigma_0 = c(1),
          sigma_1 = c(1)
        )
    )

    expect_no_condition(
      object =
        # Grid of Values
        information_to_n_difference_means(
          information = 5,
          sigma_0 = c(1, 2),
          sigma_1 = c(1, 2)
        )
    )
  }
)




test_that(
  desc = "Error Handling Works - Binary: Risk Difference",
  code = {
    expect_error(
      object =
        information_to_n_risk_difference(
          information = -1,
          pi_0 = c(0.1, 0.15),
          pi_1 = c(0.2, 0.25)
        ),
      regexp = "`information` must be positive"
    )

    expect_error(
      object =
        information_to_n_risk_difference(
          information = 50,
          pi_0 = c(0.1, 0.15),
          pi_1 = c(0.2, 0.25),
          delta = 0.1
        ),
      regexp = "Only two of the following"
    )

    expect_error(
      object =
        information_to_n_risk_difference(
          information = 50,
          pi_0 = c(0.1, 0.15),
          delta = c(-0.2, -0.25)
        ),
      regexp = "should be in the interval"
    )

    expect_error(
      object =
        information_to_n_risk_difference(
          information = 50,
          pi_0 = c(0.9, 0.95),
          delta = c(0.2, 0.25)
        ),
      regexp = "should be in the interval"
    )

    expect_error(
      object =
        information_to_n_risk_difference(
          information = 50,
          pi_0 = c(1.1, -0.10),
          delta = c(0.2, 0.25)
        ),
      regexp = "should be in the interval"
    )

    expect_error(
      object =
        information_to_n_risk_difference(
          information = 50,
          pi_1 = c(1.1, -0.10),
          delta = c(0.2, 0.25)
        ),
      regexp = "should be in the interval"
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        information_to_n_risk_difference(
          information = 50,
          pi_0 = c(0.1),
          pi_1 = c(0.3)
        )
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        information_to_n_risk_difference(
          information = 50,
          pi_1 = c(0.1),
          delta = c(-0.2)
        )
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        information_to_n_risk_difference(
          information = 50,
          pi_0 = c(0.1),
          delta = c(0.2)
        )
    )

    expect_no_condition(
      object =
        # Grid of Values
        information_to_n_risk_difference(
          information = 50,
          pi_0 = c(0.1, 0.15),
          pi_1 = c(0.2, 0.25)
        )
    )

    expect_no_condition(
      object =
        # Grid of Values
        information_to_n_risk_difference(
          information = 50,
          pi_1 = c(0.2, 0.25),
          delta = c(0.05, 0.1)
        )
    )

    expect_no_condition(
      object =
        # Grid of Values
        information_to_n_risk_difference(
          information = 50,
          pi_1 = c(0.2, 0.25),
          delta = c(-0.05, -0.1)
        )
    )
  }
)




test_that(
  desc = "Error Handling Works - Binary: Relative Risk",
  code = {
    expect_error(
      object =
        information_to_n_relative_risk(
          information = -1,
          pi_0 = c(0.1, 0.15),
          pi_1 = c(0.2, 0.25)
        ),
      regexp = "`information` must be positive"
    )

    expect_error(
      object =
        information_to_n_relative_risk(
          information = 50,
          pi_0 = c(0.1, 0.15),
          pi_1 = c(0.2, 0.25),
          rr = 0.1
        ),
      regexp = "Only two of the following"
    )

    expect_error(
      object =
        information_to_n_relative_risk(
          information = 50,
          pi_0 = c(0.1, 0.15),
          rr = c(-0.2, -0.25)
        ),
      regexp = "should be in the interval"
    )

    expect_error(
      object =
        information_to_n_relative_risk(
          information = 50,
          pi_0 = c(0.9, 0.95),
          rr = c(1.5)
        ),
      regexp = "should be in the interval"
    )

    expect_error(
      object =
        information_to_n_relative_risk(
          information = 50,
          pi_1 = c(0.9, 0.95),
          rr = c(0.5)
        ),
      regexp = "should be in the interval"
    )

    expect_error(
      object =
        information_to_n_relative_risk(
          information = 50,
          pi_0 = c(1.1, -0.10),
          rr = c(0.2, 0.25)
        ),
      regexp = "should be in the interval"
    )

    expect_error(
      object =
        information_to_n_relative_risk(
          information = 50,
          pi_1 = c(1.1, -0.10),
          rr = c(0.2, 0.25)
        ),
      regexp = "should be in the interval"
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        information_to_n_relative_risk(
          information = 50,
          pi_0 = c(0.1),
          pi_1 = c(0.3)
        )
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        information_to_n_relative_risk(
          information = 50,
          pi_1 = c(0.1),
          rr = c(2)
        )
    )

    expect_no_condition(
      object =
        # Single Parameter Set
        information_to_n_relative_risk(
          information = 50,
          pi_0 = c(0.1),
          rr = c(2)
        )
    )

    expect_no_condition(
      object =
        # Grid of Values
        information_to_n_relative_risk(
          information = 50,
          pi_0 = c(0.1, 0.15),
          pi_1 = c(0.2, 0.25)
        )
    )

    expect_no_condition(
      object =
        # Grid of Values
        information_to_n_relative_risk(
          information = 50,
          pi_1 = c(0.2, 0.25),
          rr = c(1.5, 2)
        )
    )

    expect_no_condition(
      object =
        # Grid of Values
        information_to_n_relative_risk(
          information = 50,
          pi_1 = c(0.2, 0.25),
          rr = c(1.5, 2)
        )
    )
  }
)
