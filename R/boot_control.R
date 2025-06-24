#' Parameters for using boot::boot() in Non-Sequential Analyses
#'
#' These functions are convenience functions for supplying parameters to
#' bootstrap-based analyses. \strong{NOTE}: sequential analyses require
#' computing the covariance of estimates using a bootstrap function that
#' preserves the covariance inherent in group sequential designs. This function
#' should not be used for group sequential designs.
#'
#' @param bootstrap_n A \code{numeric} scalar indicating the number of bootstrap
#' replicates to perform.
#' @param parallel A \code{character} scalar indicating whether and how to
#' use parallel computing: see \code{?\link[boot]{boot}}
#' @param ncpus A \code{numeric} scalar indicating the number of cores to use in
#' parallel computing: see \code{?\link[boot]{boot}}
#' @param ... Other named parameters passed to \code{\link[boot]{boot}}
#'
#' @name boot_control
#'
#' @return A list containing the default parameters or supplied alternatives.
#'
#' @seealso [parallel::parSapplyLB()] and [parallel::parSapply()] for parallel
#' computing; [impart::monitored_analysis] for conducting information monitored
#' analyses, which uses [impart::estimate_information] and
#' [impart::calculate_covariance()] for computing the covariance of estimates.
#'
#' @examples
#' boot_control()
#' boot_control_testing()

#' @rdname boot_control
#' @export
boot_control <-
  function(
    bootstrap_n = 10000,
    parallel = "no",
    ncpus = getOption("boot.ncpus", 1L),
    ...
  ){
    return(
      c(
        list(
          R = bootstrap_n,
          parallel = parallel,
          ncpus = ncpus
        ),
        list(...)
      )
    )
  }


#' @rdname boot_control
#' @export

boot_control_testing <-
  function(bootstrap_n = 1000, ...){
    boot_control(
      bootstrap_n = bootstrap_n,
      ...
    )
  }
