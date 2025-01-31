test_that(
  desc = "Error Handling Works",
  code = {
    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
          y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
          y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
          family = gaussian(link = "identity"),
          estimand = "difference",
          outcome_indicator_column = ".r_4",
          treatment_column = "tx"
        ),
      regexp = "Either `outcome_formula` should be specified or both"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
          family = gaussian(link = "identity"),
          estimand = "difference_does_not_exist",
          outcome_indicator_column = ".r_4",
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
          outcome_indicator_column = ".r_4",
          treatment_column = "tx_does_not_exist"
        ),
      regexp = "`treatment_column` \\(tx_does_not_exist\\)"
    )

    expect_error(
      object =
        standardization(
          data = example_1_ia_1,
          outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
          family = gaussian(link = "identity"),
          estimand = "difference",
          outcome_indicator_column = ".r_4_does_not_exist",
          treatment_column = "tx"
        ),
      regexp = "`treatment_column` \\(tx\\) and `outcome_indicator_column`"
    )
  }
)



test_that(
  desc = "Complete Data - Continuous",
  code = {
    # Continuous - Single Model: Main Effect of Treatment
    expect_no_condition(
      object = {
        result <-
          standardization(
            data = example_1_ia_1,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = gaussian(link = "identity"),
            estimand = "difference",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_no_condition(
      object = {
        result <-
          standardization(
            data = example_1_ia_1,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = gaussian(link = "identity"),
            estimand = "ratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )


    # Continuous - Single Model: Main Effect of Treatment + Interaction
    expect_no_condition(
      object = {
        result <-
          standardization(
            data = example_1_ia_1,
            outcome_formula = y_4 ~ x_1:tx + x_2:tx + x_3 + x_4 + tx,
            family = gaussian(link = "identity"),
            estimand = "difference",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_no_condition(
      object = {
        result <-
          standardization(
            data = example_1_ia_1,
            outcome_formula = y_4 ~ x_1:tx + x_2:tx + x_3 + x_4 + tx,
            family = gaussian(link = "identity"),
            estimand = "ratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )


    # Continuous - Stratified Outcome Models
    expect_no_condition(
      object = {
        result <-
          standardization(
            data = example_1_ia_1,
            y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            family = gaussian(link = "identity"),
            estimand = "difference",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_no_condition(
      object = {
        result <-
          standardization(
            data = example_1_ia_1,
            y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            family = gaussian(link = "identity"),
            estimand = "ratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )
  }
)




test_that(
  desc = "Complete Data - Binary",
  code = {

    example_1_ia_1_binary <- example_1_ia_1
    example_1_ia_1_binary$y_4 <-
      1*(example_1_ia_1_binary$y_4 > 0)

    # Binary - Single Model: Main Effect of Treatment
    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "difference",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "ratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "oddsratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )


    # Binary - Single Model: Main Effect of Treatment + Interaction
    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary,
            outcome_formula = y_4 ~ x_1:tx + x_2:tx + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "difference",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary,
            outcome_formula = y_4 ~ x_1:tx + x_2:tx + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "ratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary,
            outcome_formula = y_4 ~ x_1:tx + x_2:tx + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "oddsratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )


    # Binary - Stratified Outcome Models
    expect_true(
      object = {
        result <-
        standardization(
          data = example_1_ia_1_binary,
          y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
          y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
          family = binomial(link = "logit"),
          estimand = "difference",
          outcome_indicator_column = ".r_4",
          treatment_column = "tx"
        )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary,
            y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            family = binomial(link = "logit"),
            estimand = "ratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary,
            y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            family = binomial(link = "logit"),
            estimand = "oddsratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )
  }
)




test_that(
  desc = "Missing Covariates",
  code = {
    set.seed(seed = 12345)
    example_1_ia_1_mcar <- example_1_ia_1
    miss_rows <- sample(x = 1:nrow(example_1_ia_1_mcar), size = 10)
    example_1_ia_1_mcar$x_1[miss_rows] <- NA
    example_1_ia_1_mcar$x_2[miss_rows] <- NA
    example_1_ia_1_mcar$x_3[miss_rows] <- NA

    example_1_ia_1_binary_mcar <- example_1_ia_1_mcar
    example_1_ia_1_binary_mcar$y_4 <-
      1*(example_1_ia_1_binary_mcar$y_4 > 0)

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_mcar,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = gaussian(link = "identity"),
            estimand = "difference",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_mcar,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = gaussian(link = "identity"),
            estimand = "ratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary_mcar,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "difference",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary_mcar,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "ratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )

    expect_true(
      object = {
        result <-
          standardization(
            data = example_1_ia_1_binary_mcar,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            family = binomial(link = "logit"),
            estimand = "oddsratio",
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result$estimate)
      }
    )
  }
)




test_that(
  desc = "Test Correction Factor",
  code = {

    expect_error(
      object = {
        result <-
          standardization_correction(
            data = example_1_ia_1,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )
      },
      regex = "Either `outcome_formula` should be specified"
    )

    expect_true(
      object = {
        result <-
          standardization_correction(
            data = example_1_ia_1,
            outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result)
      }
    )

    expect_true(
      object = {
        result <-
          standardization_correction(
            data = example_1_ia_1,
            y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
            outcome_indicator_column = ".r_4",
            treatment_column = "tx"
          )

        !is.na(result)
      }
    )

  }
)
