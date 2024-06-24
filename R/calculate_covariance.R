#' Compute bootstrap estimates of covariance
#'
#' @param data A \code{data.frame} containing the data to be analyzed. A column named
#' `.id` indicates which observations correspond to each individual.
#' @param ids_by_analysis A \code{list} containing the IDs that were enrolled
#' before each analysis occurred
#' @param bootstrap_ids A \code{list} of IDs resampled at earlier analyses: this is
#' NULL in the first analysis
#' @param bootstrap_results A numeric matrix of estimates: the bootstrap
#' estimates for each analysis is stored in the corresponding column.
#' @param estimation_function A function whose arguments include a data.frame
#' named \code{data}
#' @param estimation_arguments A \code{list} of any additional arguments needed by
#' \code{estimation_function}
#' @param rng_seed A numeric scalar for seeding the L'Ecuyer pseudorandom number
#' generator
#' @param control A \code{list} of computing arguments. See
#' [impart::monitored_analysis_control] for details about the default computing
#' arguments.
#'
#' @return A \code{list} containing the covariance matrix of estimates, a matrix
#' of bootstrap estimates, and a list of the
#' IDs of individuals in each bootstrap replicate at each analysis.
#'
#' @export
#'
#' @seealso [impart::calculate_covariance()] for computing the covariance matrix
#' of estimators across analyses, and [impart::monitored_analysis_control] for
#' details about the default computing arguments.
#'
#' @examples
#' # To be added

calculate_covariance <-
  function(
    data,
    ids_by_analysis = NULL,
    bootstrap_ids = NULL,
    bootstrap_results = NULL,
    estimation_function,
    estimation_arguments,
    rng_seed,
    control = monitored_analysis_control()
  ){

    current_ids <- unique(data$.id)
    n_current_ids <- length(current_ids)

    if(is.null(ids_by_analysis)){
      ids_by_analysis <- list(current_ids)
      ids_to_sample <- current_ids
    } else {
      ids_to_sample <-
        setdiff(
          x = current_ids,
          y = do.call(
            what = c,
            args = ids_by_analysis
          )
        )
      ids_by_analysis[[length(ids_by_analysis) + 1]] <- ids_to_sample
    }

    if(is.null(bootstrap_ids)){
      n_k <- length(unique(data$.id))
      n_ids_to_sample <- n_k
    } else {
      n_k <- cumsum(x = sapply(X = ids_by_analysis, FUN = length))
      n_ids_to_sample <- utils::tail(x = diff(x = n_k), n = 1)
    }

    set.seed(seed = rng_seed, kind = "L'Ecuyer")
    random_seed <- .Random.seed
    k <- length(n_k)

    # Use separate RNG stream for each interim analysis
    if(k > 1){
      for(i in 2:k) parallel::nextRNGStream(seed = random_seed)
    }


    # Generate resampled IDs
    new_bootstrap_ids <-
      matrix(
        data = NA,
        ncol = control$n_bootstrap,
        nrow = n_ids_to_sample,
      ) |>
      apply(
        MARGIN = 2,
        FUN = function(x, ids = ids_to_sample, n = n_ids_to_sample)
          sample(
            x = ids,
            size = n,
            replace = TRUE
          )
      )

    # Combine previous and current bootstrap IDs
    all_bootstrap_ids <-
      rbind(
        sapply(
          X = bootstrap_ids,
          FUN = function(x) do.call(what = c, args = x)
        ),
        new_bootstrap_ids
      )

    # Create cluster, export objects
    cluster <- parallel::makeCluster(control$n_cores)

    # Run estimation function on bootstrapped data
    apply_function <-
      function(
    x,
    original_data = data,
    args = estimation_arguments,
    fun = estimation_function,
    run_function = calculate_estimate
      ) {
        boot_ids <- x[[1]]

        run_function(
          data =
            resample_by_id(
              data = original_data,
              ids_to_sample = boot_ids
              ),
          estimation_function = fun,
          estimation_arguments = args
        )
      }

    parallel::clusterExport(
      cl = cluster,
      varlist = c("data", "calculate_estimate",
                  "estimation_function", "estimation_arguments",
                  "apply_function"),
      envir = environment()
    )

    # Choose appropriate call depending on whether load balancing is desired
    par_apply_function <-
      ifelse(
        test = control$use_load_balancing,
        yes = parallel::parSapplyLB,
        no = parallel::parSapply
      )

    # Compute estimates in parallel
    estimates <-
      do.call(
        what = par_apply_function,
        args =
          list(
            cl = cluster,
            X =
              apply(
                X = all_bootstrap_ids,
                MARGIN = 2,
                FUN = list
              ),
            FUN = apply_function
          )
      ) |>
      as.numeric()

    # Stop the cluster
    parallel::stopCluster(cl = cluster)

    if(is.null(bootstrap_ids)){
      bootstrap_ids <- list()
      length(bootstrap_ids) <- control$n_bootstrap
    }

    # Add bootstrap IDs for analysis k
    for(i in 1:length(bootstrap_ids)){
      bootstrap_ids[[i]][[k]] <- new_bootstrap_ids[, i]
    }

    estimates <- cbind(bootstrap_results, estimates)

    return(
      list(
        covariance = stats::cov(estimates),
        ids_by_analysis = ids_by_analysis,
        bootstrap_estimates = estimates,
        bootstrap_ids = bootstrap_ids
      )
    )
  }
