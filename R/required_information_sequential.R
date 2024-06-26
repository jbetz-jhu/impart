#' Adjust Information for Interim Monitoring
#'
#' Group Sequential Design methods can control the Familywise Type I Error
#' (FWER) due to multiple assessments of efficacy. The method used to adjust
#' the sample size of a single stage design to account for multiplicity can
#' also be used to adjust the information level of an information monitored
#' design.
#'
#' @param information_fixed A numeric scalar containing the information level
#' for an information monitored design with a single efficacy analysis, created
#' using [impart::required_information_fixed_n()]
#' @param trial_design An object of type \code{trialDesignGroupSequential}
#' created by [rpact::getDesignGroupSequential()] containing a group sequential
#' design specification
#'
#' @return A scalar information level which accounts for the pre-planned
#' analyses specified in \code{gsd_design_specification}
#'
#' @export
#'
#' @seealso [impart::required_information_fixed_n()] and
#' [impart::required_information_mw_fixed_n()] for obtaining
#' \code{information_fixed}; [rpact::getDesignGroupSequential()] for
#' obtaining \code{gsd_design_specification}.
#'
#' @examples
#' # To be added

required_information_sequential <-
  function(
    information_fixed,
    trial_design
  ) {
    inflation_factor <-
      rpact::getDesignCharacteristics(trial_design)$inflationFactor
    return(information_fixed*inflation_factor)
  }
