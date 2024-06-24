#' Perform pre-specified analyses for an interim monitored trial design
#'
#' @param data A data.frame containing the data to be analyzed. A column named
#' `.id` indicates which observations correspond to each individual.
#' @param monitored_design An object of class \code{monitored_design} created
#' using [impart::initialize_monitored_design()]
#' @param estimation_function A function whose arguments include a data.frame
#' named \code{data}
#' @param estimation_arguments A list of any additional arguments needed by
#' \code{estimation_function}
#' @param correction_function A function which takes the arguments supplied to
#' \code{estimation_function} and returns a numeric scalar which performs a
#' small sample correction to the variance estimate
#' @param control A list containing the control arguments for computation,
#' typically created with [impart::monitored_analysis_control()]
#' @param ... Additional arguments, currently ignored
#'
#' @return An object of class \code{monitored_design}
#'
#' @export
#'
#' @examples
#' # To be added

monitored_analysis <-
  function(
    data,
    monitored_design = NULL,
    estimation_function,
    estimation_arguments,
    correction_function = NULL,
    control = monitored_analysis_control(),
    ...
  ){

    data <- as.data.frame(data)
    all_args <- c(as.list(environment()), list(...))

    # Check for valid input
    do.call(
      what = monitored_design_checks,
      args =
        all_args[
          intersect(
            x = names(all_args),
            y = methods::formalArgs(monitored_design_checks)
          )
        ]
    )

    if(is.null(monitored_design)) {
      stop("Initialize an information monitored design with ",
           "initialize_monitored_design()")
    } else {
      information_target <- monitored_design$original_design$information_target
      trial_design <- utils::tail(x = monitored_design, n = 1)[[1]]$trial_design
      orthogonalize <- monitored_design$original_design$orthogonalize
      rng_seed <- monitored_design$original_design$rng_seed
    }

    estimated_information <-
      estimate_information(
        data = data,
        monitored_design = monitored_design,
        estimation_function = estimation_function,
        estimation_arguments = estimation_arguments,
        orthogonalize = orthogonalize,
        rng_seed = rng_seed,
        control = control,
        return_results = TRUE
      )

    estimates <- estimate_information$estimates
    covariance <- estimate_information$covariance
    information <- estimate_information$information
    estimates_orthogonal <- estimate_information$estimates_orthogonal
    covariance_orthogonal <- estimate_information$covariance_orthogonal
    information_orthogonal <- estimate_information$information_orthogonal
    ids_by_analysis <- estimate_information$ids_by_analysis
    bootstrap_results <- estimate_information$bootstrap_results
    bootstrap_ids <- estimate_information$bootstrap_ids
    analysis_label <- estimate_information$analysis_label

    information_fraction <- information/information_target
    information_fraction_orthogonal <-
      information_orthogonal/information_target

    z_statistics <- estimates/sqrt(diag(covariance))
    z_statistics_orthogonal <-
      estimates_orthogonal/sqrt(diag(covariance_orthogonal))

    if(orthogonalize){
      interim_decisions <-
        apply_stopping_rule_z(
          test_statistics = z_statistics_orthogonal,
          trial_design = trial_design,
          information_fraction = information_fraction_orthogonal,
          information_target = information_target
        )
    } else {
      interim_decisions <-
        apply_stopping_rule_z(
          test_statistics = z_statistics,
          trial_design = trial_design,
          information_fraction = information_fraction,
          information_target = information_target
        )
    }

    continue <- interim_decisions$continue
    decision <- interim_decisions$decision
    decision_detail <- interim_decisions$decision_detail
    stopping_stage <- interim_decisions$stopping_stage
    trial_design_updated <- interim_decisions$trial_design_updated
    decision_data <- interim_decisions$decision_data

    analysis_results <-
      c(monitored_design,
        stats::setNames(
          object =
            list(
              list(
                data = data,
                trial_design = trial_design_updated,
                information_target = information_target,
                estimates = estimates,
                covariance = covariance,
                z_statistics = z_statistics,
                information = information,
                information_fraction = information_fraction,
                estimates_orthogonal = estimates_orthogonal,
                covariance_orthogonal = covariance_orthogonal,
                z_statistics_orthogonal = z_statistics_orthogonal,
                information_orthogonal = information_orthogonal,
                information_fraction_orthogonal =
                  information_fraction_orthogonal,
                ids_by_analysis = ids_by_analysis,
                bootstrap_results = bootstrap_results,
                bootstrap_ids = bootstrap_ids,
                continue = continue,
                decision = decision,
                decision_detail = decision_detail,
                decision_data = decision_data,
                session_info = utils::sessionInfo()
              )
            ),
          nm = estimated_information$analysis_label
        )
      )

    class(analysis_results) <- "monitored_design"

    return(analysis_results)
  }
