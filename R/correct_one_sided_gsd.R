#' Correct one-sided trialDesignGroupSequential object
#'
#' Currently in [rpact::getDesignGroupSequential()], when the test is one-sided,
#' there is no way to specify the direction of the null and alternative
#' hypotheses. This is an
#' [open issue in rpact.](https://github.com/rpact-com/rpact/issues/26). This
#' correction is only necessary for using `rpact` and `impart` together or when
#' exporting the correct boundaries from a one-sided
#' `trialDesignGroupSequential` object when the alternative is less than the
#' null value.
#'
#' @param trial_design A `trialDesignGroupSequential` which was called with the
#' argument `sided = 1` (i.e. one-sided alternative).
#' @param higher_better Logical scalar: Should the alternative be
#' theta > theta_0 (TRUE) or theta < theta_0 (FALSE) or
#'
#' @return A `trialDesignGroupSequential` object with the efficacy and futility
#' boundaries corrected and a logical scalar `directionUpper` indicating whether
#' the alternative is greater than the null value (TRUE) or less than the null
#' value (FALSE).
#'
#' @export correct_one_sided_gsd
#'
#' @examples
#' # One sided test: Higher values = Better
#' trial_design_one_sided_higher <-
#'   rpact::getDesignGroupSequential(
#'     alpha = 0.05,
#'     beta = 1 - 0.80,
#'     sided = 1,
#'     informationRates = c(0.50, 0.75, 1),
#'     typeOfDesign = "asOF",
#'     typeBetaSpending = "bsOF",
#'     bindingFutility = FALSE
#'   )
#'
#' # Bounds assumed are for $H_{A}: \theta > \theta_{0}$
#' trial_design_one_sided_higher$criticalValues
#' trial_design_one_sided_higher$futilityBounds
#'
#' # One sided test: Lower values = Better
#' trial_design_one_sided_lower <-
#'   correct_one_sided_gsd(
#'     trial_design = trial_design_one_sided_higher,
#'     higher_better = FALSE
#'   )
#'
#' # Bounds for $H_{A}: \theta < \theta_{0}$
#' trial_design_one_sided_lower$criticalValues
#' trial_design_one_sided_lower$futilityBounds

correct_one_sided_gsd <-
  function(
    trial_design,
    higher_better = TRUE
  ){

    new_design <- rlang::env_clone(env = trial_design)

    if(trial_design$sided == 1){
      if(!higher_better){
        # Flip boundaries
        new_design$criticalValues <- -trial_design$criticalValues
        new_design$futilityBounds <- -trial_design$futilityBounds
      }

      new_design$directionUpper <- higher_better
    }

    class(new_design) <- class(trial_design)
    return(new_design)
  }
