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

    estimates <- estimated_information$estimates
    covariance_uncorrected <- estimated_information$covariance_uncorrected
    variance <- estimated_information$variance
    information <- estimated_information$information
    correction_factor <- estimated_information$correction_factor
    information_uncorrected <- estimated_information$information_uncorrected
    estimates_orthogonal <- estimated_information$estimates_orthogonal
    covariance_orthogonal <- estimated_information$covariance_orthogonal_uncorrected
    variance_orthogonal <- estimated_information$variance_orthogonal
    information_orthogonal <- estimated_information$information_orthogonal
    covariance_orthogonal_uncorrected <-
      estimated_information$covariance_orthogonal_uncorrected
    information_orthogonal_uncorrected <-
      estimated_information$information_orthogonal_uncorrected

    ids_by_analysis <- estimated_information$ids_by_analysis
    bootstrap_results <- estimated_information$bootstrap_results
    bootstrap_ids <- estimated_information$bootstrap_ids
    analysis_label <- estimated_information$analysis_label

    information_fraction <- information/information_target
    information_fraction_orthogonal <-
      information_orthogonal/information_target

    z_statistics <- estimates*sqrt(information)
    z_statistics_orthogonal <-
      estimates_orthogonal*sqrt(information_orthogonal)

    if(orthogonalize){
      analysis_decisions <-
        analysis_decisions_orthogonal <-
        apply_stopping_rule_z(
          test_statistics = z_statistics_orthogonal,
          trial_design = trial_design,
          information_fraction = information_fraction_orthogonal,
          information_target = information_target
        )
    } else {
      analysis_decisions_orthogonal <- NA
    }

    if((!orthogonalize) | control$orthogonal_and_oblique) {
      analysis_decisions_oblique <-
        apply_stopping_rule_z(
          test_statistics = z_statistics,
          trial_design = trial_design,
          information_fraction = information_fraction,
          information_target = information_target
        )

      if(!orthogonalize) {
        analysis_decisions <- analysis_decisions_oblique
      }
    } else {
      analysis_decisions_oblique <- NA
    }

    continue <- analysis_decisions$continue
    decision <- analysis_decisions$decision
    decision_detail <- analysis_decisions$decision_detail
    stopping_stage <- analysis_decisions$stopping_stage
    trial_design_updated <- analysis_decisions$trial_design_updated
    decision_data <- analysis_decisions$decision_data

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
                covariance_uncorrected = covariance_uncorrected,
                variance = variance,
                z_statistics = z_statistics,
                information = information,
                information_fraction = information_fraction,
                information_uncorrected = information_uncorrected,
                estimates_orthogonal = estimates_orthogonal,
                covariance_orthogonal_uncorrected =
                  covariance_orthogonal_uncorrected,
                variance_orthogonal = variance_orthogonal,
                z_statistics_orthogonal = z_statistics_orthogonal,
                information_orthogonal = information_orthogonal,
                information_fraction_orthogonal =
                  information_fraction_orthogonal,
                information_orthogonal_uncorrected =
                  information_orthogonal_uncorrected,
                ids_by_analysis = ids_by_analysis,
                bootstrap_results = bootstrap_results,
                bootstrap_ids = bootstrap_ids,
                continue = continue,
                decision = decision,
                decision_detail = decision_detail,
                decision_data = decision_data,
                analysis_decisions_oblique = analysis_decisions_oblique,
                analysis_decisions_orthogonal = analysis_decisions_orthogonal,
                session_info = utils::sessionInfo()
              )
            ),
          nm = estimated_information$analysis_label
        )
      )

    class(analysis_results) <- "monitored_design"

    return(analysis_results)
  }
