test_that(
  desc = "Unadjusted, Not Orthogonalized",
  code = {
    ### Unadjusted, Not Orthognalized, No `monitored_design` ###################
    expect_no_condition(
      info_unadjusted_not_orthogonalized_no_monitored_design <-
        estimate_information(
          data = example_1_ia_1,
          monitored_design = NULL,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ 1,
              y1_formula = y_4 ~ 1,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = FALSE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )
    )

    expect_contains(
      object = names(info_unadjusted_not_orthogonalized_no_monitored_design),
      expected = c("estimates", "covariance_uncorrected", "variance",
                   "information", "correction_factor")
    )


    ### Unadjusted, Not Orthognalized, with `monitored_design` #################
    expect_no_condition(
      info_unadjusted_not_orthogonalized_w_monitored_design <-
        estimate_information(
          data = example_1_ia_1,
          monitored_design = monitored_design,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ 1,
              y1_formula = y_4 ~ 1,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = FALSE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )
    )

    expect_contains(
      object = names(info_unadjusted_not_orthogonalized_w_monitored_design),
      expected = c("estimates", "covariance_uncorrected", "variance",
                   "information", "correction_factor")
    )


    ### Unadjusted, Not Orthognalized: Stage 2 #################################
    expect_no_condition(
      interim_analysis_1 <-
        monitored_analysis(
          data = example_1_ia_1,
          monitored_design = monitored_design,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ 1,
              y1_formula = y_4 ~ 1,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = FALSE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )
    )

    expect_contains(
      object = names(interim_analysis_1),
      expected = c("original_design", "interim_analysis_1")
    )

    expect_no_condition(
      info_unadjusted_not_orthogonalized_w_monitored_design_s2 <-
        estimate_information(
          data = example_1_ia_2,
          monitored_design = interim_analysis_1,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ 1,
              y1_formula = y_4 ~ 1,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = FALSE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )
    )

    expect_contains(
      object = names(info_unadjusted_not_orthogonalized_w_monitored_design_s2),
      expected = c("estimates", "covariance_uncorrected", "variance",
                   "information", "correction_factor")
    )
  }
)




test_that(
  desc = "Adjusted, Orthogonalized",
  code = {
    ### Adjusted, Orthognalized, No `monitored_design` #########################
    expect_no_condition(
      info_adjusted_orthogonalized_no_monitored_design <-
        estimate_information(
          data = example_1_ia_1,
          monitored_design = NULL,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = TRUE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )
    )

    expect_contains(
      object = names(info_adjusted_orthogonalized_no_monitored_design),
      expected = c("estimates", "covariance_uncorrected", "variance",
                   "information", "correction_factor")
    )


    ### Adjusted, Orthognalized, with `monitored_design` #######################
    expect_no_condition(
      info_adjusted_orthogonalized_w_monitored_design <-
        estimate_information(
          data = example_1_ia_1,
          monitored_design = monitored_design,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = TRUE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )
    )

    expect_contains(
      object = names(info_adjusted_orthogonalized_w_monitored_design),
      expected = c("estimates", "covariance_uncorrected", "variance",
                   "information", "correction_factor")
    )


    ### Adjusted, Orthognalized: Stage 2 #######################################
    expect_no_condition(
      interim_analysis_1 <-
        monitored_analysis(
          data = example_1_ia_1,
          monitored_design = monitored_design,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = TRUE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )
    )

    expect_contains(
      object = names(interim_analysis_1),
      expected = c("original_design", "interim_analysis_1")
    )

    expect_no_condition(
      info_adjusted_orthogonalized_w_monitored_design_s2 <-
        estimate_information(
          data = example_1_ia_2,
          monitored_design = interim_analysis_1,
          estimation_function = standardization,
          estimation_arguments =
            list(
              estimand = "difference",
              y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
              family = gaussian,
              treatment_column = "tx",
              outcome_indicator_column = ".r_4"
            ),
          correction_function = standardization_correction,
          orthogonalize = TRUE,
          rng_seed = 12345,
          control = monitored_analysis_control_testing()
        )
    )

    expect_contains(
      object = names(info_adjusted_orthogonalized_w_monitored_design_s2),
      expected = c("estimates", "covariance_uncorrected", "variance",
                   "information", "correction_factor")
    )
  }
)
