#' Adjust Information Target for a Group Sequential Design
#'
#' Group Sequential Design methods can control the Familywise Type I Error
#' (FWER) due to multiple assessments of efficacy and allow for futility
#' stopping. Interim stopping can potentially reduce the average length of a
#' study, often requiring only a modest increase in the maximum potential sample
#' size. The method used to adjust the sample size of a single stage design
#' to account for multiplicity can also be used to adjust the information target
#' of an information monitored design.
#'
#' It is often advantageous to perform pre-planned interim analyses during a
#' trial to assess whether the efficacy of treatments has already been
#' established, or whether it is futile to continue collecting data. Group
#' Sequential Designs are a principled way of carrying out such studies that
#' control familywise type I error. The use of \eqn{\alpha}-spending functions
#' allow for stopping boundaries to be adjusted to the amount of data collected.
#'
#' When there are assessments for efficacy or futility, achieving the same level
#' of power and type I error requires an increase in sample size. This
#' "inflation factor" depends on the number of analyses (often denoted \eqn{K})
#' and type of stopping boundaries chosen (often denoted as a "shape parameter"
#' \eqn{\Delta}).
#'
#' \code{get_gsd_inflation_factor} extracts the "inflation factor" from a
#' group sequential design created by [rpact::getDesignGroupSequential].
#' \code{required_information_sequential} takes in an information target
#' \eqn{\mathcal{I}} for a single-stage design (no interim analyses of
#' efficacy), and a group sequential design specification, and returns an
#' information target \eqn{\mathcal{I}_{GSD}} for an information monitored
#' design with group sequential stopping for efficacy and/or futility.
#'
#' @param information_single_stage A numeric scalar containing the information level
#' for an information monitored design with a single efficacy analysis, created
#' using [required_information_single_stage]
#' @param trial_design An object of type \code{trialDesignGroupSequential}
#' created by [rpact::getDesignGroupSequential] containing a group sequential
#' design specification
#'
#' @name inflation_factor
#'
#' @return \code{get_gsd_inflation_factor} returns a \code{numeric} scalar
#' containing the inflation factor, and \code{required_information_sequential}
#' returns a \code{numeric} scalar containing the information target accounting
#' for the pre-planned analyses specified in \code{trial_design}.
#'
#' @export
#'
#' @seealso [required_information_single_stage] and
#' [required_information_mw_single_stage] for obtaining
#' \code{information_single_stage}; [rpact::getDesignGroupSequential()] for
#' obtaining \code{trial_design}.
#'
#' @references {
#' Mehta, CR, and Tsiatis AA. 2001. "Flexible Sample Size Considerations Using
#' Information-Based Interim Monitoring". \emph{Drug Information Journal}
#' 35 (4): 1095–1112. \url{https://doi.org/10.1177/009286150103500407}
#'
#' Jennison, C, and Turnbull, BW. 1999. \emph{Group Sequential Methods with
#' Applications to Clinical Trials}. Chapman; Hall/CRC.
#' \url{https://doi.org/10.1201/9780367805326}.
#'
#' Wassmer, G, and Pahlke, F. 2025. \emph{Rpact: Confirmatory Adaptive Clinical
#' Trial Design and Analysis}.
#' \url{https://doi.org/10.32614/CRAN.package.rpact}.
#' }
#'
#' @examples
#' # To be added




#' @rdname inflation_factor
#' @export
required_information_sequential <-
  function(
    information_single_stage,
    trial_design
  ) {
    inflation_factor <-
      get_gsd_inflation_factor(trial_design)
    return(information_single_stage*inflation_factor)
  }




#' @rdname inflation_factor
#' @export
get_gsd_inflation_factor <-
  function(trial_design){
    return(rpact::getDesignCharacteristics(trial_design)$inflationFactor)
  }
