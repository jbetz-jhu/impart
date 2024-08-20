#' Control arguments for performing information monitored analyses
#'
#' This provides a way to set and modify the analysis parameters for information
#' monitoring.
#'
#' @param orthogonal_and_oblique A logical scalar: if `orthogonal == TRUE`
#' should non-orthogonalized (oblique) test statistics and decisions be
#' returned?
#' @param required_packages A character vector of required packages to be
#' loaded on a cluster: only used when `n_cores` > 1.
#' @param n_bootstrap Scalar number containing the number of bootstrap
#' replicates to perform
#' @param n_cores Scalar number of cores to use.
#' @param use_load_balancing Logical scalar: Should load balancing be used?
#'
#' @name monitored_analysis_control
#'
#' @return A list containing the default parameters or supplied alternatives.
#'
#' @seealso [parallel::parSapplyLB()] and [parallel::parSapply()] for parallel
#' computing; [impart::monitored_analysis] for conducting information monitored
#' analyses, which uses [impart::estimate_information] and
#' [impart::calculate_covariance()] for computing the covariance of estimates.
#'
#' @examples
#' # To be added


#' @rdname monitored_analysis_control
#' @export
monitored_analysis_control <-
  function(
    orthogonal_and_oblique = FALSE,
    required_packages = character(0),
    n_bootstrap = 1000,
    n_cores = 1,
    use_load_balancing = FALSE
  ){
    return(
      list(
        orthogonal_and_oblique = FALSE,
        n_bootstrap = n_bootstrap,
        n_cores = n_cores,
        use_load_balancing = use_load_balancing,
        required_packages = required_packages
      )
    )
  }




#' @rdname monitored_analysis_control
#' @export
monitored_analysis_control_testing <-
  function(
    orthogonal_and_oblique = FALSE,
    required_packages = character(0),
    n_bootstrap = 250,
    n_cores = 1,
    use_load_balancing = FALSE
  ){
    return(
      list(
        orthogonal_and_oblique = FALSE,
        n_bootstrap = n_bootstrap,
        n_cores = n_cores,
        use_load_balancing = use_load_balancing,
        required_packages = required_packages
      )
    )
  }
