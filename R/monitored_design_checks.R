#' Check consistency of a \code{monitored_design} object
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
#'
#' @return No result is returned.
#'
#' @export
#'
#' @examples
#' # To be added

monitored_design_checks <-
  function(
    data,
    monitored_design = NULL,
    estimation_function,
    estimation_arguments,
    correction_function = NULL
  ) {

    if(!(".id" %in% names(data))){
      stop("Data must contain a column `.id` identifying all rows belonging ",
           "to each individual.")
    }

    information_target <- monitored_design$original_design$information_target

    prior_analysis <- utils::tail(x = monitored_design, 1)[[1]]

    if(is.null(trial_design) & is.null(monitored_design)){
      stop("For an initial analysis, `trial_design` must be specified. For ",
           "subsequent analyses, `monitored_design` must be specified.")
    } else if(!(is.null(trial_design) | is.null(monitored_design))){
      previous_design <- prior_analysis$trial_design
      compare_params <-
        c("kMax", "alpha", "beta", "sided",
          "typeOfDesign", "gammaA","typeBetaSpending", "gammaB")
      if(
        !identical(
          x = as.list(trial_design)[compare_params],
          y = as.list(previous_design)[compare_params]
        )
      ){
        stop("`trial_design` must be identical to `trial_design` in the latest ",
             "analysis in `monitored_design` for the following parameters: ",
             paste(compare_params, collapse = ", "))
      }
    } else if(is.null(trial_design) & !is.null(monitored_design)){
      trial_design <- prior_analysis$trial_design
    }

    # Check previous analyses against current design
    if(!is.null(monitored_design)){

      previous_decisions <-
        sapply(
          X = monitored_design[-1],
          FUN = function(x) get(x = "decision", pos = x)
        )

      if(any(!previous_decisions %in% c("continue"))){
        if(any(previous_decisions %in% c("futility"))){
          warning("Futility stopping boundary reached at analyses: ",
                  paste(which(previous_decisions == "futility"), collapse = ", ")
          )
        }

        if(any(previous_decisions %in% c("efficacy"))){
          stop("Efficacy stopping boundary reached at analyses: ",
               paste(which(previous_decisions == "efficacy"), collapse = ", "))
        }

        if(any(previous_decisions %in% c("fail to reject"))){
          stop("All pre-specified analyses completed: Failed to reject null ",
               "hypothesis at final analysis.")
        }
      }

      previous_designs <-
        lapply(
          X = monitored_design,
          FUN = function(y) get(x = "trial_design", pos = y)
        )

      k <-
        setdiff(
          x = names(monitored_design),
          y = "original_design"
        ) |> length()

      # Get information rates: Arrange each into column of matrix
      informationRates <-
        sapply(
          X = previous_designs,
          FUN = function(y) get(x = "informationRates", pos = y)
        )

      if(is.list(informationRates)){
        stop("Information rates should have the same length across all ",
             "analyses. `informationRates` in `monitored_design` have ",
             "lengths: ",
             paste0(sapply(X = informationRates, FUN = length), collapse = ", ")
        )
      }

      # Check consistency of observed information
      if(k > 1){
        for(i in 1:(k - 1)){
          if(length(unique(utils::tail(x = informationRates[i, ], -i))) > 1){
            stop("Inconsistent observed information fractions for interim ",
                 "analysis ", i, ".")
          }
        }
      }


      # Check consistency of design parameters
      check_params <-
        c("alpha", "beta", "sided", "typeOfDesign",
          "typeBetaSpending", "bindingFutility")

      previous_params <-
        sapply(
          X = previous_designs,
          FUN = function(x)
            unlist(mget(x = check_params, envir = as.environment(x)))
        )

      previous_param_conflicts <-
        previous_params |>
        apply(
          MARGIN = 1,
          FUN = unique
        ) |>
        sapply(
          FUN = length
        )

      if(any(previous_param_conflicts > 1)){
        stop("Conflicting values in analysis parameters for ",
             paste(paste0("`", names(which(previous_param_conflicts > 1)), "`"),
                   collapse = ", "), ".")
      }

      if(k > 1){
        # Check consistency of design parameters
        check_results <-
          c("estimates", "estimatesOrthogonal",
            "variance", "varianceOrthogonal",
            "testStatistics", "testStatisticsOrthogonal",
            "decision", "decisionOrthogonal")

        # Determine which results are contained in the object
        check_results <-
          lapply(X = utils::tail(x = monitored_design, -1), FUN = names) |>
          unlist() |>
          intersect(y = check_results)

        previous_results <-
          lapply(
            X = utils::tail(x = monitored_design, -1),
            FUN = function(x)
              mget(x = check_results, envir = as.environment(x))
          )

        for(i in check_results){
          extracted_result <-
            sapply(
              X = previous_results,
              FUN =
                function(x, get_result = i, new_length = k){
                  extracted_result <-
                    get(x = i, pos = x)
                  length(extracted_result) <- new_length
                  return(extracted_result)
                }
            )

          if(any(!is.na(extracted_result[lower.tri(extracted_result)]))){
            stop("Inconsistency in monitored_design element `", result, "`.")
          }

          unique_results <-
            apply(
              X = extracted_result,
              MARGIN = 1,
              FUN = function(x) length(unique(stats::na.omit(object = x)))
            )

          if(any(unique_results > 1)){
            stop("Inconsistency in monitored_design element `", result, "`.")
          }
        }
      }
    }
  }
