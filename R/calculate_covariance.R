#' Compute bootstrap covariance of estimates
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
    if(is.null(data$.id)){
      stop("data must have a subject id column named data$.id")
    }

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
      n_k <- n_ids_to_sample <- n_current_ids
      bootstrap_ids <- list()
    } else {
      n_k <- cumsum(x = sapply(X = ids_by_analysis, FUN = length))
      n_ids_to_sample <- utils::tail(x = diff(x = n_k), n = 1)
    }

    set.seed(seed = rng_seed, kind = "L'Ecuyer")
    random_seed <- .Random.seed
    k <- length(n_k)

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

    bootstrap_ids[[k]] <- new_bootstrap_ids

    all_bootstrap_ids <- do.call(what = rbind, args = bootstrap_ids)

    if(control$n_cores == 1){
      estimates <-
        compute_bootstrap_serial(
          data = data,
          ids = all_bootstrap_ids,
          estimation_function = estimation_function,
          estimation_arguments = estimation_arguments
        )
    } else if(control$n_cores > 1) {
      estimates <-
        compute_bootstrap_parallel(
          data = data,
          ids = all_bootstrap_ids,
          estimation_function = estimation_function,
          estimation_arguments = estimation_arguments,
          n_cores = control$n_cores,
          use_load_balancing = control$use_load_balancing,
          required_packages = control$required_packages
        )
    }

    estimates <- cbind(bootstrap_results, estimates)

    return(
      list(
        covariance = stats::cov(estimates),
        ids_by_analysis = ids_by_analysis,
        bootstrap_estimates = estimates,
        bootstrap_ids = bootstrap_ids,
        random_seed = random_seed
      )
    )
  }
