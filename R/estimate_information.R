#' Estimate the observed information level
#'
#' This is a function for computing the information level from a dataset. For
#' multi-stage designs, earlier results can be passed as a
#' \code{monitored_design} object.
#'
#' @param data A data.frame containing the data to be analyzed. A column named
#' `.id` indicates which observations correspond to each individual.
#' @param monitored_design An object of class \code{monitored_design} created
#' using [impart::initialize_monitored_design()]
#' @param estimation_function A function whose arguments include a data.frame
#' named \code{data}
#' @param estimation_arguments A list of any additional arguments needed by
#' \code{estimation_function}
#' @param orthogonalize Logical scalar: Should estimates, their covariance,
#' and the resulting test statistics be orthogonalized?
#' @param rng_seed Numeric scalar containing the L'Ecuyer pseudorandom
#' number generator seed
#' @param return_results Logical scalar: Should estimates be returned, or only
#' the covariance and information levels?
#' @param control A list containing the control arguments for computation,
#' typically created with [impart::monitored_analysis_control()]
#'
#' @return A list containing the following elements:
#'
#' @seealso [impart::calculate_covariance()] for computing the covariance matrix
#' of estimators across analyses, [impart::monitored_analysis_control] for
#' details about the default computing arguments, [impart::monitored_analysis]
#' for conducting analyses at pre-specified information levels
#'
#' @export
#'
#' @examples
#' # To be added

estimate_information <-
  function(
    data,
    monitored_design,
    estimation_function,
    estimation_arguments,
    orthogonalize = NULL,
    rng_seed,
    return_results = FALSE,
    control = monitored_analysis_control()
  ) {

    # If previous analyses specified, assign most recent result to trial_design
    if(!is.null(monitored_design)){
      k <- setdiff(x = names(monitored_design), y = "original_design") |>
        length() + 1

      n_analyses <- monitored_design$original_design$trial_design$kMax

      prior_analysis <- utils::tail(monitored_design, 1)[[1]]

      orthogonalize <- monitored_design$original_design$orthogonalize
      rng_seed <- monitored_design$original_design$rng_seed
      information_target <- monitored_design$original_design$information_target

      trial_design <- prior_analysis$trial_design
      ids_by_analysis <- prior_analysis$ids_by_analysis
      estimates <- prior_analysis$estimates
      covariance <- prior_analysis$covariance
      estimates_orthogonal <- prior_analysis$estimates_orthogonal
      covariance_orthogonal <- prior_analysis$covariance_orthogonal
      bootstrap_ids <- prior_analysis$bootstrap_ids
      bootstrap_results <- prior_analysis$bootstrap_results
      information <- prior_analysis$information

      rm(prior_analysis)

    } else {
      k <- 1
      n_analyses <- 1
      dataset_list <- list(data)
      bootstrap_ids <- NULL
      bootstrap_results <- NULL
      estimates <- NULL
      covariance <- NULL
      estimates_orthogonal <- NULL
      covariance_orthogonal <- NULL
      ids_by_analysis <- NULL
    }


    if(is.null(monitored_design)){
      analysis_label <- "monitoring_check"
    } else {
      if (k == n_analyses){
        analysis_label <- "final_analysis"
      } else{
        analysis_label <- paste0("interim_analysis_", k)
      }

      n_analyses <- trial_design$kMax
    }

    k <- length(estimates) + 1

    estimate_k <-
      calculate_estimate(
        data = data,
        estimation_function = estimation_function,
        estimation_arguments = estimation_arguments
      )

    covariance_k <-
      calculate_covariance(
        data = data,
        ids_by_analysis = ids_by_analysis,
        bootstrap_ids = bootstrap_ids,
        bootstrap_results = bootstrap_results,
        estimation_function = estimation_function,
        estimation_arguments = estimation_arguments,
        rng_seed = rng_seed,
        control = control
      )

    estimates <- as.numeric(c(estimates, estimate_k))
    covariance <- covariance_k$covariance
    information <- 1/diag(covariance)

    # Add to Previous Estimates
    if(orthogonalize == TRUE){
      if(k == 1) {
        estimates_orthogonal <- estimate_k
        covariance_orthogonal <- covariance_k$covariance
      } else {
        orthogonalized_k <-
          orthogonalize_estimates(
            estimates = estimates,
            covariance = covariance_k$covariance
          )

        estimates_orthogonal[k] <- orthogonalized_k$estimate_orthogonal

        covariance_orthogonal <-
          c(diag(covariance_orthogonal), orthogonalized_k$covariance_orthogonal)

        covariance_orthogonal <-
          sqrt(covariance_orthogonal) %*% t(sqrt(covariance_orthogonal))
      }

      information_orthogonal <- 1/diag(covariance_orthogonal)
    } else {
      estimates_orthogonal <- NA*estimates
      covariance_orthogonal <- NA*covariance$covariance
    }

    if(return_results){
      bootstrap_ids <- covariance_k$bootstrap_ids
      bootstrap_results <- covariance_k$bootstrap_estimates
      ids_by_analysis <- covariance_k$ids_by_analysis
    } else {
      estimates <- estimates_orthogonal <-
        bootstrap_ids <- bootstrap_results <- ids_by_analysis <- NULL
    }

    return(
      list(
        estimates = estimates,
        covariance = covariance,
        information = information,
        estimates_orthogonal = estimates_orthogonal,
        covariance_orthogonal = covariance_orthogonal,
        information_orthogonal = information_orthogonal,
        ids_by_analysis = ids_by_analysis,
        bootstrap_results = bootstrap_results,
        bootstrap_ids = bootstrap_ids,
        analysis_label = analysis_label
      )
    )
  }
