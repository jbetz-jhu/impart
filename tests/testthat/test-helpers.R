test_that(
  desc = "Helpers Load",
  code = {
    expect_equal(
      object = exists(x = "trial_design"),
      expected = TRUE
    )

    expect_equal(
      object = exists(x = "information_adaptive"),
      expected = TRUE
    )

    expect_equal(
      object = exists(x = "monitored_design"),
      expected = TRUE
    )
  }
)
