
# [X] 1. Input Error Handling
# [X] 2. Two-Sided Tests: No Futility Stopping (OF) - 3 Stages
# [X] 3. Two-Sided Tests: Non-Binding Futility (OF, OF) - 3 Stages
# [X] 4. One-Sided Tests Higher: No Futility Stopping (OF) - 3 Stages
# [X] 5. One-Sided Tests Higher: Non-Binding Futility Stopping (OF) - 3 Stages
# [X] 6. One-Sided Tests Lower: No Futility Stopping (OF) - 3 Stages
# [X] 7. One-Sided Tests Lower: Non-Binding Futility Stopping (OF) - 3 Stages

of_efficacy_no_futility_2S_3_stage <-
  rpact::getDesignGroupSequential(
    alpha = 0.05, # 5% FWER
    beta = 1 - 0.80, # 80% Power
    sided = 2,
    typeOfDesign = "asOF",
    informationRates = c(0.50, 0.75, 1)
  )

of_efficacy_of_futility_2S_3_stage <-
  rpact::getDesignGroupSequential(
    alpha = 0.05, # 5% FWER
    beta = 1 - 0.80, # 80% Power
    sided = 2,
    typeOfDesign = "asOF",
    typeBetaSpending = "bsOF",
    informationRates = c(0.50, 0.75, 1),
    bindingFutility = FALSE
  )

of_efficacy_no_futility_1s_higher_3_stage <-
  rpact::getDesignGroupSequential(
    alpha = 0.05, # 5% FWER
    beta = 1 - 0.80, # 80% Power
    sided = 1,
    typeOfDesign = "asOF",
    informationRates = c(0.50, 0.75, 1)
  )

of_efficacy_of_futility_1s_higher_3_stage <-
  rpact::getDesignGroupSequential(
    alpha = 0.05, # 5% FWER
    beta = 1 - 0.80, # 80% Power
    sided = 1,
    typeOfDesign = "asOF",
    typeBetaSpending = "bsOF",
    informationRates = c(0.50, 0.75, 1),
    bindingFutility = FALSE
  )




of_efficacy_no_futility_1s_lower_3_stage <-
  correct_one_sided_gsd(
    trial_design = of_efficacy_no_futility_1s_higher_3_stage,
    higher_better = FALSE
  )

of_efficacy_of_futility_1s_lower_3_stage <-
  correct_one_sided_gsd(
    trial_design = of_efficacy_of_futility_1s_higher_3_stage,
    higher_better = FALSE
  )

of_efficacy_no_futility_1s_higher_3_stage <-
  correct_one_sided_gsd(
    trial_design = of_efficacy_no_futility_1s_higher_3_stage,
    higher_better = TRUE
  )

of_efficacy_of_futility_1s_higher_3_stage <-
  correct_one_sided_gsd(
    trial_design = of_efficacy_of_futility_1s_higher_3_stage,
    higher_better = TRUE
  )




# 1. Input Error Handling ######################################################
testthat::test_that(
  desc = "1. Input Error Handling",
  code = {
    ## 1.1 Statistics Longer than Information Fraction #########################
    testthat::expect_error(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_2S_3_stage$criticalValues[1:2] +
                c(-0.1, 0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      regexp = "Length of `test_statistics`"
    )

    ## 1.2 Information Fraction longer than Statistics #########################
    testthat::expect_error(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_2S_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      regexp = "Length of `test_statistics`"
    )

    ## 1.3 Pre-specified number of tests exceeded ##############################
    testthat::expect_error(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                c(of_efficacy_of_futility_2S_3_stage$criticalValues, 4) +
                c(-0.1, -0.1, -0.1, -0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1, 1.5),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      regexp = "pre-specified number of analyses"
    )
  }
)




# 2. Two-Sided Tests: No Futility Stopping (OF) - 3 Stages #####################
testthat::test_that(
  desc = "2. Two-Sided: No Futility",
  code = {
    ## 2.1 Above 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_2S_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Efficacy: Upper")
    )

    ## 2.2 Below 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_2S_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Efficacy: Lower")
    )

    ## 2.3 Within 1st Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_2S_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_2S_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 2.4 Above 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_2S_3_stage$criticalValues[1:2] +
                c(-0.1, 0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Efficacy: Upper")
    )

    ## 2.5 Below 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_2S_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Efficacy: Lower")
    )

    ## 2.6 Within 2nd Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_2S_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_2S_3_stage$criticalValues[1:2] +
                c(0.1, 0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 2.7 Above 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_2S_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, 0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Efficacy: Upper")
    )

    ## 2.8 Below 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_2S_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, -0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Efficacy: Lower")
    )

    ## 2.9 Fail To Reject #####################################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_2S_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, -0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_2S_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, 0.1),
              trial_design = of_efficacy_no_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )
  }
)




# 3. Two-Sided Tests: Non-Binding Futility (OF, OF) - 3 Stages #################
testthat::test_that(
  desc = "3. Two-Sided: Non-Binding OF Futility",
  code = {
    ## 3.1 Above 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_of_futility_2S_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Efficacy: Upper")
    )

    ## 3.2 Below 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_of_futility_2S_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Efficacy: Lower")
    )

    ## 3.3.1 In 1st Futility Range #############################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_of_futility_2S_3_stage$futilityBounds[1] +
                c(-0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Futility")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_of_futility_2S_3_stage$futilityBounds[1] +
                c(0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Futility")
    )

    ## 3.4 Within 1st Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_of_futility_2S_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_of_futility_2S_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 3.5 Above 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_of_futility_2S_3_stage$criticalValues[1:2] +
                c(-0.1, 0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Efficacy: Upper")
    )

    ## 3.6 Below 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_of_futility_2S_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Efficacy: Lower")
    )

    ## 3.7.1 In 2nd Futility Range #############################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_of_futility_2S_3_stage$futilityBounds[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Futility")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_of_futility_2S_3_stage$futilityBounds[1:2] +
                c(-0.1, 0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Futility")
    )

    ## 3.8 Within 2nd Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_of_futility_2S_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_of_futility_2S_3_stage$criticalValues[1:2] +
                c(0.1, 0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 3.9 Above 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_of_futility_2S_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, 0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Efficacy: Upper")
    )

    ## 3.10 Below 3rd Critical Value ###########################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_of_futility_2S_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, -0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Efficacy: Lower")
    )

    ## 3.11 Fail To Reject #####################################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_of_futility_2S_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, -0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_of_futility_2S_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, 0.1),
              trial_design = of_efficacy_of_futility_2S_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )
  }
)
















### 4. One-Sided Tests Higher: No Futility Stopping (OF) - 3 Stages ############
testthat::test_that(
  desc = "4. One-Sided Tests Higher: No Futility Stopping (OF) - 3 Stages",
  code = {
    ## 4.1 Above 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Efficacy: Upper")
    )

    ## 4.2 Below 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 4.3 Within 1st Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 4.4 Above 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:2] +
                c(-0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Efficacy: Upper")
    )

    ## 4.5 Below 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 4.6 Within 2nd Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:2] +
                c(0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 4.7 Above 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Efficacy: Upper")
    )

    ## 2.8 Below 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )

    ## 4.9 Fail To Reject #####################################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )
  }
)




### 5. One-Sided Tests Higher: Non-Binding Futility Stopping (OF) - 3 Stages ####
testthat::test_that(
  desc = "4. One-Sided Tests Higher: No Futility Stopping (OF) - 3 Stages",
  code = {
    ## 4.1 Above 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Efficacy: Upper")
    )

    ## 4.2 Below 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 4.3 Within 1st Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 4.4 Above 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:2] +
                c(-0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Efficacy: Upper")
    )

    ## 4.5 Below 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 4.6 Within 2nd Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:2] +
                c(0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 4.7 Above 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Efficacy: Upper")
    )

    ## 2.8 Below 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )

    ## 4.9 Fail To Reject #####################################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_higher_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_higher_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )
  }
)




### 6. One-Sided Tests Lower: No Futility Stopping (OF) - 3 Stages #############
testthat::test_that(
  desc = "6. One-Sided Tests Lower: No Futility Stopping (OF) - 3 Stages",
  code = {
    ## 6.1 Above 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1] + c(0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 6.2 Below 1st Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1] + c(-0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "1", "Efficacy: Lower")
    )

    ## 6.3 Within 1st Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1] +
                c(-0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1] +
                c(0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = 0.5,
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 6.4 Above 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1:2] + c(0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 6.5 Below 2nd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1:2] +
                c(0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "2", "Efficacy: Lower")
    )

    ## 6.6 Within 2nd Critical Values ##########################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1:2] +
                c(-0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1:2] +
                c(0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = c(0.5, 0.75),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("TRUE", NA, "Continue")
    )

    ## 6.7 Above 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1:3] + c(0.1, 0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Fail to reject")
    )

    ## 6.8 Below 3rd Critical Value ############################################
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", "3", "Efficacy: Lower")
    )

    ## 6.9 Fail to Reject ######################################################
    # Positive
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                -of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1:3] +
                c(-0.1, -0.1, -0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", 3, "Fail to reject")
    )

    # Negative
    testthat::expect_equal(
      object =
        with(
          data =
            apply_stopping_rule_z(
              test_statistics =
                of_efficacy_no_futility_1s_lower_3_stage$criticalValues[1:3] +
                c(0.1, 0.1, 0.1),
              trial_design = of_efficacy_no_futility_1s_lower_3_stage,
              information_fraction = c(0.5, 0.75, 1.00),
              information_target = 10
            ),
          expr = c(continue, stopping_stage, decision)
        ),
      expected = c("FALSE", 3, "Fail to reject")
    )

  }
)



### 7. One-Sided Tests Lower: Non-Binding Futility Stopping (OF) - 3 Stages ####
