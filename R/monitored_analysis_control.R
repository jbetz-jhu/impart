#' Control arguments for performing information monitored analyses
#'
#' This provides a way to set and modify the analysis parameters for information
#' monitoring.
#'
#' @param required_packages A character vector of required packages to be
#' loaded on a cluster: only used when `n_cores` > 1.
#' @param n_bootstrap Scalar number containing the number of bootstrap
#' replicates to perform
#' @param n_cores Scalar number of cores to use.
#' @param use_load_balancing Logical scalar: Should load balancing be used?
#'
#' @return A list containing the specified inputs.
#'
#' @export
#'
#' @seealso [parallel::parSapplyLB()] and [parallel::parSapply()] for parallel
#' computing; [impart::monitored_analysis] for conducting information monitored
#' analyses, which uses [impart::estimate_information] and
#' [impart::calculate_covariance()] for computing the covariance of estimates.
#'
#' @examples
#' # To be added

monitored_analysis_control <-
  function(
    required_packages = NULL,
    n_bootstrap = 1000,
    n_cores = 1,
    use_load_balancing = FALSE
  ){
    return(
      list(
        n_bootstrap = n_bootstrap,
        n_cores = n_cores,
        use_load_balancing = use_load_balancing,
        required_packages = required_packages
      )
    )
  }
