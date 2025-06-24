test_that(
  desc = "Error Handling Works",
  code = {

    expect_error(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = "y_4 ~ tx + x_1 + x_2 + x_3 + x_4",
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "`outcome_formula` must be a formula"
    )

    expect_error(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = "tx ~ 1",
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "`treatment_formula` must be a formula"
    )

    expect_error(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = ".r_4 ~ tx + x_1 + x_2 + x_3 + x_4",
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "`missing_formula` must be a formula"
    )

    expect_error(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "ddifference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "`estimand` must be one of the following"
    )

    expect_error(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "nnone",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "`se_method` must be one of the following"
    )

    expect_error(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = yy_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "`formula` contains variables not included in `data`"
    )

    expect_error(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "must be in `outcome_formula`"
    )

    expect_error(
      object =
        dr_joffe(
          data =
            within(
              data = example_1_ia_1,
              expr = {tx[1:4] <- NA}
            ),
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "Treatment assignment \\(.*\\) should not be missing"
    )

    # missing_formula indicates outcome observed, yet value is missing
    expect_error(
      object =
        dr_joffe(
          data =
            within(
              data = example_1_ia_1,
              expr = {
                y_4[which(.r_4 == 1)[1:3]] <- NA
              }
            ),
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "Missing outcome indicated as observed"
    )

    # missing_formula indicates outcome not observed, yet value is observed
    expect_error(
      object =
        dr_joffe(
          data =
            within(
              data = example_1_ia_1,
              expr = {
                y_4[which(.r_4 == 0)[1:3]] <- 1:3
              }
            ),
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "Non-missing outcome indicated as missing"
    )

    # Outcome indicated as not-yet-observed, but not missing
    expect_error(
      object =
        dr_joffe(
          data =
            within(
              data = example_1_ia_1,
              expr = {
                y_4[which(is.na(.r_4))[1:3]] <- 1:3
              }
            ),
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        ),
      regexp = "Non-missing outcome indicated as missing"
    )
  }
)


test_that(
  desc = "Works with valid input",
  code = {

    expect_no_condition(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = FALSE
        )
    )

    expect_no_condition(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "none",
          alpha = 0.05,
          verbose = TRUE
        )
    )

    expect_no_condition(
      object =
        dr_joffe(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          outcome_family = gaussian,
          treatment_formula = tx ~ 1,
          missing_formula = .r_4 ~ tx + x_1 + x_2 + x_3 + x_4,
          missing_family = quasibinomial,
          estimand = "difference",
          se_method = "bootstrap",
          bootstrap_parameters = boot_control_testing(),
          alpha = 0.05,
          verbose = TRUE
        )
    )
  }
)
