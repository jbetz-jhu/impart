#' Initialize an Information Monitoring Design
#'
#' Rather than planning data collection until a pre-specified sample size is
#' reached based on estimates of nuisance parameters, an information-monitored
#' study continues data collection until they provide sufficient precision to
#' identify a meaningful difference with appropriate power and control of Type I
#' Error. The design is specified in terms of an information level rather than
#' a fixed sample size, and recruitment is stopped when the accrued participants
#' are projected to meet the required level of precision to test the hypotheses
#' of interest.
#'     This function creates an object that encapsulates all aspects of the
#' study design that must be specified in advance, such as the sequential
#' analysis plan, the value of the estimand under the null, the maximum sample
#' size allowed to be enrolled before recruitment is automatically suspended,
#' the target information level, whether test statistics should be
#' orthogonalized, and the pseudorandom number generator seeds to be used in
#' analyses.
#'
#' @param trial_design An object of type \code{trialDesignGroupSequential}
#' created by [rpact::getDesignGroupSequential()] containing a group sequential
#' design specification
#' @param null_value Numeric scalar containing the value of the estimand under
#' the null hypothesis, e.g. 0 for differences or 1 for ratios
#' @param maximum_sample_size Numeric scalar containing the maximum sample size
#' after which recruitment will be stopped if it has not already been stopped
#' from information monitoring.
#' @param information_target A numeric scalar containing the information level
#' for an information monitored design created
#' using [impart::required_information_single_stage()] and adjusted for multiplicity
#' using [impart::required_information_sequential()]
#' @param orthogonalize Logical scalar: Should estimates, their covariance,
#' and the resulting test statistics be orthogonalized?
#' @param rng_seed_analysis Numeric scalar containing the L'Ecuyer pseudorandom
#' number generator seed to be used for the analyses specified in
#' \code{trial_design}
#'
#' @return A "monitored design" object which is used in several `impart`
#' functions.
#'
#' @export
#'
#' @examples
#' # To be added


initialize_monitored_design <-
  function(
    trial_design,
    null_value = NULL,
    maximum_sample_size,
    information_target,
    orthogonalize,
    rng_seed_analysis
  ){

    if(is.null(null_value)){
      stop("Null value of estimand must be specified.")
    } else if(!all(is.finite(trial_design$informationRates))){
      stop("`trial_design$informationRates` must be specified.")
    } else if(!trial_design$typeOfDesign %in% c("asP", "asOF", "asKD", "asHSD")){
      stop("`trial_design$informationRates` must be one of the following: ",
           "\"asP\", \"asOF\", \"asKD\", or \"asHSD\"")
    } else if(!trial_design$typeBetaSpending %in%
              c("none", "bsP", "bsOF", "bsKD", "bsHSD")){
      stop("`trial_design$typeBetaSpending` must be one of the following: ",
           "\"none\", \"bsP\", \"bsOF\", \"bsKD\", or \"bsHSD\"")
    } else if(trial_design$bindingFutility){
      stop("`bindingFutility` must be set to `FALSE`")
    } else if(
      !all(is.finite(maximum_sample_size), is.finite(information_target),
           is.finite(rng_seed_analysis))
    ){
      stop("The following parameters must be finite numeric values:",
           "`maximum_sample_size`, `information_target`, `rng_seed_analysis`")
    } else if(!orthogonalize %in% c(FALSE, TRUE)){
      stop("Orthogonalized must be either `FALSE` or `TRUE`.")
    }


    monitored_design <-
      stats::setNames(
        object = list(
          list(
            trial_design = trial_design,
            maximum_sample_size = maximum_sample_size,
            null_value = null_value,
            information_target = information_target,
            orthogonalize = orthogonalize,
            rng_seed_analysis = rng_seed_analysis
          )
        ),
        nm = "original_design"
      )
    class(monitored_design) <- "monitored_design"
    return(monitored_design)
  }
