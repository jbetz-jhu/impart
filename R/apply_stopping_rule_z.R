#' Apply group sequential stopping rules to test statistics
#'
#' Group sequential designs allow investigators to perform pre-planned interim
#' assessments of futility or efficacy in an ongoing trial while preserving the
#' Familywise Type I Error Rate (FWER). Differences between the observed
#' information levels and those specified in the initial design require the
#' boundaries to be updated in order to preserve FWER control. This function
#' allows users to update the boundaries according to observed information
#' levels, apply stopping rules, and return their resulting decisions.
#'
#' @param test_statistics A numeric vector of standardized test statistics
#' @param trial_design A \code{TrialDesignGroupSequential object} created by
#' [rpact::getDesignGroupSequential()]
#' @param information_fraction A numeric vector containing the observed
#' information fractions
#' @param information_target A numeric scalar containing the target information
#' level, created by [impart::required_information_fixed_n()]
#'
#' @return A list containing an updated \code{TrialDesignGroupSequential object}
#' object, the resulting decision, and supporting data.
#'
#' @export
#'
#' @seealso [impart::required_information_fixed_n()] and
#' [impart::required_information_mw_fixed_n()] for obtaining the target
#' information level for a single stage design;
#' [rpact::getDesignGroupSequential()] for obtaining \code{trial_design}, and
#' [impart::required_information_sequential()] for adjusting the information
#' level to multi-stage designs.
#'
#'
#'
#' @examples
#'
#' # Two-sided design with O'Brien-Fleming efficacy stopping and non-binding
#' # O'Brien Fleming Beta Spending Futility with 2 interim analyses at 50% and
#' # 75%
#'
#' # Create Initial `TrialDesignGroupSequential` Object:
#' two_sided_of_efficacy_nb_of_futility_0 <-
#'  rpact::getDesignGroupSequential(
#'    alpha = 0.05, # 5% Type I Error
#'    beta = 1 - 0.80, # 80% Power
#'    sided = 2, # Two-Sided Test
#'    # Interim analyses at 50%, 75%, 100% of Information Target
#'    informationRates = c(0.5, 0.75, 1),
#'    # Efficacy and Non-Binding Futility Stopping using O'Brien-Fleming
#'    # alpha- and beta- spending functions
#'    typeOfDesign = "asOF",
#'    typeBetaSpending = "bsOF",
#'    bindingFutility = FALSE
#'  )
#'
#'  # Test Statistics at Interim Analysis 1
#'  test_statistics_1 <- c(1.0)
#'
#'  # Observed information fraction at 1st analysis = 52.5% vs. 50% in design
#'  information_fraction_1 <- c(0.525)
#'  information_target <- 50
#'
#'  analysis_decision_1 <-
#'    apply_stopping_rule_z(
#'      test_statistics = test_statistics_1,
#'      trial_design = two_sided_of_efficacy_nb_of_futility_0,
#'      information_fraction = information_fraction_1,
#'      information_target = information_target
#'    )
#'
#'  # Test Statistics at Interim Analysis 2
#'  test_statistics_2 <- c(1.0, 2.2)
#'
#'  # Observed information fraction at 2nd analysis = 74.5% vs. 75% in design
#'  information_fraction_2 <- c(0.525, 0.745)
#'
#'  analysis_decision_2 <-
#'    apply_stopping_rule_z(
#'      test_statistics = test_statistics_2,
#'      trial_design =
#'      # Use updated design from analysis 1
#'      analysis_decision_1$trial_design_updated,
#'      information_fraction = information_fraction_2,
#'      information_target = information_target
#'  )
#'
#'  # Test Statistics at Interim Analysis 3
#'  test_statistics_3 <- c(1.0, 2.2, 2.3)
#'
#'  # Observed information fraction at 3rd analysis = 102% vs. 100% in design
#'  information_fraction_3 <- c(0.525, 0.745, 1.02)
#'  analysis_decision_3 <-
#'    apply_stopping_rule_z(
#'      test_statistics = test_statistics_3,
#'      trial_design =
#'      # Use updated design from analysis 1
#'      analysis_decision_2$trial_design_updated,
#'      information_fraction = information_fraction_3,
#'      information_target = information_target
#'  )

apply_stopping_rule_z <-
  function(
    test_statistics,
    trial_design,
    information_fraction,
    information_target
  ){

    if(length(test_statistics) != length(information_fraction)){
      stop("Length of `test_statistics` (", length(test_statistics), ") and ",
           "`information_fraction` (", length(information_fraction), ") must ",
           "be identical")
    }

    k <- length(test_statistics)
    k_max <- trial_design$kMax
    two_sided <- trial_design$sided == 2

    if(k > k_max){
      stop("Length of `test_statistics` (", k, ") greater than the ",
           "pre-specified number of analyses (", k_max, ")")
    }

    if(two_sided){
      ha_greater <- NA
    } else {
      ha_greater <- trial_design$directionUpper
    }

    if(k < k_max){
      # Change information level from a priori value to observed value
      information_rates <- trial_design$informationRates
      information_rates[k] <- information_fraction[k]

      trial_design_params_interim <-
        c("kMax", "alpha", "beta", "sided",
          "typeOfDesign", "gammaA","typeBetaSpending", "gammaB")

      trial_design_updated <-
        do.call(
          what = rpact::getDesignGroupSequential,
          args =
            c(as.list(trial_design)[trial_design_params_interim],
              list(informationRates = information_rates))
        )

    } else if (k == k_max){
      information_rates <- information_fraction*information_target
      information_rates <-
        information_rates/utils::tail(x = information_rates, 1)

      trial_design_params_final <-
        c("kMax", "alpha", "beta", "sided", "typeBetaSpending", "gammaB")

      trial_design_updated <-
        do.call(
          what = rpact::getDesignGroupSequential,
          args =
            c(as.list(trial_design)[trial_design_params_final],
              list(
                typeOfDesign = "asUser",
                informationRates = information_rates,
                userAlphaSpending = trial_design$alphaSpent
              )
            )
        )
    }

    bounds_efficacy <- trial_design_updated$criticalValues
    bounds_futility <- trial_design_updated$futilityBounds

    futility_bound_k <-
      ifelse(
        test = !is.na(bounds_futility[k]),
        yes = bounds_futility[k],
        no = -Inf
      )

    continue <- NA

    # Efficacy Stopping
    if(two_sided | ha_greater){
      if(test_statistics[k] > bounds_efficacy[k]){
        continue <- FALSE
        decision <- "Efficacy: Upper"
        decision_detail <- paste0("Efficacy: Upper - stage ", k, " of ", k_max)
        stopping_stage <- k
      }
    }

    if(two_sided | !ha_greater){
      if(test_statistics[k] < -bounds_efficacy[k]){
        continue <- FALSE
        decision <- "Efficacy: Lower"
        decision_detail <- paste0("Efficacy: Lower - stage ", k, " of ", k_max)
        stopping_stage <- k
      }
    }

    # Futility Stopping
    if(is.na(continue) & (k < k_max)){
      if(two_sided){
        if(
          (test_statistics[k] > -futility_bound_k) &
          (test_statistics[k] < futility_bound_k)
        ){
          continue <- FALSE
          decision <- "Futility"
          decision_detail <- paste0("Futility - stage ", k, " of ", k_max)
          stopping_stage <- k
        }
      } else if(ha_greater){
        if(test_statistics[k] < futility_bound_k){
          continue <- FALSE
          decision <- "Futility"
          decision_detail <- paste0("Futility - stage ", k, " of ", k_max)
          stopping_stage <- k
        }
      } else {
        if(test_statistics[k] > -futility_bound_k){
          continue <- FALSE
          decision <- "Futility"
          decision_detail <- paste0("Futility - stage ", k, " of ", k_max)
          stopping_stage <- k
        }
      }
    }

    if(is.na(continue)){
      if(k < k_max){
        continue <- TRUE
        decision <- "Continue"
        decision_detail <- paste0("Continue - stage ", k, " of ", k_max)
        stopping_stage <- NA
      } else {
        continue <- FALSE
        decision <- "Fail to reject"
        decision_detail <- paste0("Fail to reject: stage ", k, " of ", k_max)
        stopping_stage <- k_max
      }
    }

    if(length(test_statistics) < k_max){
      test_statistics <-
        c(test_statistics, rep(NA, k_max - length(test_statistics)))
    }

    return(
      list(
        continue = continue,
        decision = decision,
        decision_detail = decision_detail,
        stopping_stage = stopping_stage,
        trial_design_updated = trial_design_updated,
        decision_data =
          data.frame(
            test_statistic = test_statistics,
            efficacy = bounds_efficacy,
            futility = c(bounds_futility, NA)
          )
      )
    )
  }
