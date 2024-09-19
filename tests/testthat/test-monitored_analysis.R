test_that(
  desc = "Unadjusted, Not orthogonalized",
  code = {
    ### Unadjusted, Not Orthogonalized: Stage 2 ################################
    expect_no_condition(
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
        control =
          monitored_analysis_control(
            n_bootstrap = 100
          )
      )
    )


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
          control = monitored_analysis_control_testing(
            n_bootstrap = 100
          )
        )
    )

    expect_contains(
      object = names(interim_analysis_1),
      expected = c("original_design", "interim_analysis_1")
    )

    expect_no_condition(
      interim_analysis_2 <-
        monitored_analysis(
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
          control = monitored_analysis_control_testing(
            n_bootstrap = 100
          )
        )
    )

    expect_contains(
      object = names(interim_analysis_2),
      expected =
        c("original_design", "interim_analysis_1", "interim_analysis_2")
    )


    ### Unadjusted, Not Orthogonalized: Stage 3 ################################
    expect_no_condition(
      final_analysis <-
        monitored_analysis(
          data = example_1_final,
          monitored_design = interim_analysis_2,
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
          control = monitored_analysis_control_testing(
            n_bootstrap = 100
          )
        )
    )

    expect_contains(
      object = names(final_analysis),
      expected =
        c("original_design", "interim_analysis_1", "interim_analysis_2",
          "final_analysis")
    )
  }
)



test_that(
  desc = "Adjusted, Orthogonalized",
  code = {
    ### Adjusted, Orthogonalized: Stage 2 ######################################
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
          control = monitored_analysis_control_testing(
            n_bootstrap = 100
          )
        )
    )

    expect_contains(
      object = names(interim_analysis_1),
      expected = c("original_design", "interim_analysis_1")
    )

    expect_no_condition(
      interim_analysis_2 <-
        monitored_analysis(
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
          control = monitored_analysis_control_testing(
            n_bootstrap = 100
          )
        )
    )

    expect_contains(
      object = names(interim_analysis_2),
      expected =
        c("original_design", "interim_analysis_1", "interim_analysis_2")
    )


    ### Adjusted, Orthogonalized: Stage 3 ######################################
    expect_no_condition(
      final_analysis <-
        monitored_analysis(
          data = example_1_final,
          monitored_design = interim_analysis_2,
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
          control = monitored_analysis_control_testing(
            n_bootstrap = 100
          )
        )
    )

    expect_contains(
      object = names(final_analysis),
      expected =
        c("original_design", "interim_analysis_1", "interim_analysis_2",
          "final_analysis")
    )
  }
)
