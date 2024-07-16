#' Compute bootstrap from a vector of IDs
#'
#' Bootstrapping is done by resampling all rows corresponding to a resampled ID
#' to work with both long and wide format data without reshaping.
#'
#' @param data A \code{data.frame} containing the data to be analyzed. A column named
#' `.id` indicates which observations correspond to each individual.
#' @param ids A vector of IDs to resample from `data`
#' @param estimation_function A function whose arguments include a data.frame
#' named \code{data}
#' @param estimation_arguments A \code{list} of any additional arguments needed by
#' \code{estimation_function}
#'
#' @name compute_bootstrap
#'
#' @return A vector containing the results of `estimation_function` evaluated on
#' each bootstrap replicate of `data` contained in the columns of `ids`
#'
#' @export
#'
#' @seealso [impart::calculate_covariance()] for computing the covariance matrix
#' of estimators across analyses, [impart::monitored_analysis_control] for
#' details about the default computing arguments.
#'
#' @examples
#' # to be added
#'

#' @rdname compute_bootstrap
compute_bootstrap_serial <-
  function(
    data,
    ids,
    estimation_function,
    estimation_arguments
  ){

    n_bootstrap <- ncol(ids)
    estimates <- rep(NA_real_, n_bootstrap)

    for(i in 1:n_bootstrap){
      estimates[i] <-
        do.call(
          what = calculate_estimate,
          args =
            list(
              data =
                relabel_by_id(
                  data = data,
                  ids = ids[,i]
                ),
              estimation_function = estimation_function,
              estimation_arguments = estimation_arguments
            )
        )
    }

    return(estimates)
  }


#' @rdname compute_bootstrap
#' @param n_cores Scalar number of cores to use.
#' @param use_load_balancing Logical scalar: Should load balancing be used?
#' @param required_packages A character vector of required packages: extracted
#' from the control argument to [impart::calculate_covariance()]
compute_bootstrap_parallel <-
  function(
    data,
    ids,
    estimation_function = estimation_function,
    estimation_arguments = estimation_arguments,
    n_cores = 1,
    use_load_balancing = FALSE,
    required_packages = NULL
  ){
    n_bootstrap <- ncol(ids)

    # Start the cluster - Pass required packages; Load packages
    cluster <- parallel::makeCluster(n_cores)

    parallel::clusterExport(
      cl = cluster, varlist = "required_packages"
    )

    parallel::clusterEvalQ(
      cl = cluster,
      lapply(
        X = c("impart", required_packages),
        FUN = library,
        character.only = TRUE
      )
    )

    id_list <-
      lapply(seq_len(ncol(ids)), function(i) ids[,i])

    apply_function <-
      function(
    x,
    original_data = data,
    args = estimation_arguments,
    fun = estimation_function,
    run_function = calculate_estimate
      ) {
        run_function(
          data =
            relabel_by_id(
              data = original_data,
              ids = x
            ),
          estimation_function = fun,
          estimation_arguments = args
        )
      }


    # Pass environment to cluster
    parallel::clusterExport(
      cl = cluster,
      varlist = ls(),
      envir = environment()
    )

    if(use_load_balancing){
      estimates <-
        parallel::parSapplyLB(
          cl = cluster,
          X = id_list,
          FUN = apply_function
        )
    } else {
      estimates <-
        parallel::parSapply(
          cl = cluster,
          X = id_list,
          FUN = apply_function
        )
    }

    # Stop the cluster
    parallel::stopCluster(cl = cluster)

    return(estimates)
  }
