test_that(
  desc = "Error Handling Works",
  code = {
    alpha <- 0.05
    power <- 0.8
    test_sides <- 2
    mcid <- -0.10
    delta_null <- 0

    pi_0 <- 0.15
    pi_1 <- pi_0 + mcid
    h_true <- pwr::ES.h(p1 = pi_1, p2 = pi_0)
    # Shift towards 0.5
    h_conservative <- pwr::ES.h(p1 = pi_1 + 0.075, p2 = pi_0 + 0.075)

    n_per_arm <-
      pwr::pwr.2p.test(
        h = h_conservative,
        power = power,
        sig.level = alpha,
        alternative = "two.sided"
      )$n |> ceiling()

    information_single_stage <-
      required_information_single_stage(
        delta = mcid,
        delta_0 = delta_null,
        power = power,
        alpha = alpha,
        sides = test_sides
      )

    design_single_stage <-
      rpact::getDesignGroupSequential(
        informationRates = 1
      )

    expect_error(
      object =
        initialize_monitored_design(
          trial_design = design_single_stage,
          maximum_sample_size = 2*n_per_arm,
          information_target = information_single_stage,
          orthogonalize = FALSE,
          rng_seed_analysis = 12345
        ),
      regex = "Null value of estimand must be specified"
    )

    expect_error(
      object =
        initialize_monitored_design(
          trial_design =
            rpact::getDesignGroupSequential(
              alpha = alpha,
              beta = 1 - power,
              sided = 2,
              informationRates = c(0.5, 0.75, 1),
              typeOfDesign = "OF",
              typeBetaSpending = "none"
            ),
          null_value = 0,
          maximum_sample_size = 2*n_per_arm,
          information_target = information_single_stage,
          orthogonalize = FALSE,
          rng_seed_analysis = 12345
        ),
      regex = "must be one of the following: \"asP\", \"asOF\""
    )

    expect_error(
      object =
        initialize_monitored_design(
          trial_design =
            rpact::getDesignGroupSequential(
              alpha = alpha,
              beta = 1 - power,
              sided = 2,
              informationRates = c(0.5, 0.75, 1),
              typeOfDesign = "asOF",
              typeBetaSpending = "bsOF",
              bindingFutility = TRUE
            ),
          null_value = 0,
          maximum_sample_size = 2*n_per_arm,
          information_target = information_single_stage,
          orthogonalize = FALSE,
          rng_seed_analysis = 12345
        ),
      regex = "`bindingFutility` must be set to `FALSE`"
    )

    expect_error(
      object =
        initialize_monitored_design(
          trial_design = design_single_stage,
          null_value = 0,
          maximum_sample_size = Inf,
          information_target = information_single_stage,
          orthogonalize = FALSE,
          rng_seed_analysis = 12345
        ),
      regex = "The following parameters must be finite numeric values"
    )

    expect_error(
      object =
        initialize_monitored_design(
          trial_design = design_single_stage,
          null_value = 0,
          maximum_sample_size = 2*n_per_arm,
          information_target = information_single_stage,
          orthogonalize = 1,
          rng_seed_analysis = 12345
        ),
      regex = "`orthogonalize` must be either `TRUE` or `FALSE`"
    )
  }
)


test_that(
  desc = "Works with Valid Input: Risk Difference",
  code ={
    alpha <- 0.05
    power <- 0.8
    test_sides <- 2
    mcid <- -0.10
    delta_null <- 0

    pi_0 <- 0.15
    pi_1 <- pi_0 + mcid
    h_true <- pwr::ES.h(p1 = pi_1, p2 = pi_0)
    # Shift towards 0.5
    h_conservative <- pwr::ES.h(p1 = pi_1 + 0.075, p2 = pi_0 + 0.075)

    n_per_arm <-
      pwr::pwr.2p.test(
        h = h_conservative,
        power = power,
        sig.level = alpha,
        alternative = "two.sided"
      )$n |> ceiling()

    information_single_stage <-
      required_information_single_stage(
        delta = mcid,
        delta_0 = delta_null,
        power = power,
        alpha = alpha,
        sides = test_sides
      )

    design_single_stage <-
      rpact::getDesignGroupSequential(
        informationRates = 1
      )

    expect_no_condition(
      object =
        initialize_monitored_design(
          trial_design = design_single_stage,
          null_value = delta_null,
          maximum_sample_size = 2*n_per_arm,
          information_target = information_single_stage,
          orthogonalize = FALSE,
          rng_seed_analysis = 12345
        )
    )
  }
)


test_that(
  desc = "Works with Valid Input: Relative Risk",
  code ={
    alpha <- 0.05
    power <- 0.8
    test_sides <- 2
    mcid <- log(0.60)
    delta_null <- 0 # H0: RR = 1 -> log(RR) = 0

    pi_0 <- 0.15
    pi_1 <- exp(mcid)*pi_0

    n_per_arm <-
      impart::rr_design(
        pi_1 = pi_1,
        pi_0 = pi_0,
        power = power,
        alpha = alpha,
        test_sides = test_sides
      )$n_per_arm

    information_single_stage <-
      required_information_single_stage(
        delta = mcid,
        delta_0 = delta_null,
        power = power,
        alpha = alpha,
        sides = test_sides
      )

    design_single_stage <-
      rpact::getDesignGroupSequential(
        informationRates = 1
      )

    expect_no_condition(
      object =
        initialize_monitored_design(
          trial_design = design_single_stage,
          null_value = delta_null,
          maximum_sample_size = 2*n_per_arm,
          information_target = information_single_stage,
          orthogonalize = FALSE,
          rng_seed_analysis = 12345
        )
    )
  }
)

