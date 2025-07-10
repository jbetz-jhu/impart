test_that(
  desc = "Error Handling Works",
  code = {
    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
          outcome_formula_control = y_4 ~ x_1 + x_2 + x_3 + x_4,
          outcome_formula_treatment = y_4 ~ x_1 + x_2 + x_3 + x_4,
          family = gaussian(link = "identity"),
          estimand = "difference",
          treatment_column = "tx"
        ),
      regexp = "Either `outcome_formula` must be a formula, or both"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
          family = gaussian(link = "identity"),
          estimand = "difference_does_not_exist",
          treatment_column = "tx"
        ),
      regexp = "`estimand` must be one of the following"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
          family = gaussian(link = "identity"),
          estimand = "difference",
          treatment_column = "tx_does_not_exist"
        ),
      regexp = "Formula contains variables not included in `data`"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
          family = gaussian(link = "identity"),
          estimand = "difference",
          treatment_column = "tx",
          se_method = "influence_does_not_exist"
        ),
      regexp = "`se_method` must be one of the following"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx + tx*x_1,
          family = gaussian(link = "identity"),
          estimand = "difference",
          treatment_column = "tx",
          se_method = "influence"
        ),
      regexp = "Correction not available with treatment-covariate interactions"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx -1,
          family = gaussian(link = "identity"),
          estimand = "difference",
          treatment_column = "tx",
          se_method = "influence"
        ),
      regexp = "Influence function not yet implemented for models with treatment-covariate"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula_control = y_4 ~ x_1 + x_2 + x_3 + x_4,
          outcome_formula_treatment = y_4 ~ x_1 + x_2 + x_3 + x_4,
          family = gaussian(link = "identity"),
          estimand = "difference",
          treatment_column = "tx",
          se_method = "influence"
        ),
      regexp = "Influence function not yet implemented for treatment-stratified"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
          family = gaussian(link = "identity"),
          estimand = "ratio",
          treatment_column = "tx",
          se_method = "influence"
        ),
      regexp = "Correction not available for estimand"
    )
  }
)


test_that(
  desc = "Works with valid input",
  code = {

    # No standard error ########################################################
    expect_no_condition(
      standardization(
        data = example_1_ia_1,
        outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
        family = gaussian(link = "identity"),
        estimand = "difference",
        treatment_column = "tx",
        se_method = "none"
      )
    )

    expect_no_condition(
      standardization(
        data = example_1_ia_1,
        outcome_formula_control = y_4 ~ x_1 + x_2 + x_3 + x_4,
        outcome_formula_treatment = y_4 ~ x_1 + x_2 + x_3 + x_4,
        family = gaussian(link = "identity"),
        estimand = "difference",
        treatment_column = "tx",
        se_method = "none"
      )
    )


    # Bootstrap standard error #################################################
    expect_no_condition(
      standardization(
        data = example_1_ia_1,
        outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
        family = gaussian(link = "identity"),
        estimand = "difference",
        treatment_column = "tx",
        se_method = "bootstrap",
        bootstrap_parameters = impart::boot_control(bootstrap_n = 250)
      )
    )

    expect_no_error(
      suppressWarnings(
        # Expected warning: In norm.inter(t, adj.alpha) : extreme order statistics used as endpoints
        expr = {
          standardization(
            data = example_1_ia_1,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = gaussian(link = "identity"),
            estimand = "ratio",
            treatment_column = "tx",
            se_method = "bootstrap",
            variance_adjustment = NULL,
            bootstrap_parameters = impart::boot_control(bootstrap_n = 250)
          )
        }
      )
    )

    expect_no_condition(
      standardization(
        data = example_1_ia_1,
        outcome_formula_control = y_4 ~ x_1 + x_2 + x_3 + x_4,
        outcome_formula_treatment = y_4 ~ x_1 + x_2 + x_3 + x_4,
        family = gaussian(link = "identity"),
        estimand = "difference",
        treatment_column = "tx",
        se_method = "bootstrap",
        bootstrap_parameters = impart::boot_control(bootstrap_n = 250)
      )
    )

    # Influence standard error #################################################
    expect_no_condition(
      standardization(
        data = example_1_ia_1,
        outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
        family = gaussian(link = "identity"),
        estimand = "difference",
        treatment_column = "tx",
        se_method = "influence"
      )
    )

    # Score standard error #####################################################
    expect_no_condition(
      standardization(
        data = example_1_ia_1,
        outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
        family = gaussian(link = "identity"),
        estimand = "difference",
        treatment_column = "tx",
        se_method = "score"
      )
    )
  }
)
