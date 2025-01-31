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

    if(!identical(x = class(data), y = "data.frame")){
      stop("`data` must be a `data.frame`: ",
           "if `data` is a tibble (has class \"tbl_df\"), use as.data.frame(data)")
    }

    if(!(".id" %in% names(data))){
      stop("`data` must contain a column `.id` identifying all rows belonging ",
           "to each individual.")
    }

    if(is.null(monitored_design)){
      stop("`monitored_design` must be specified: ",
           "See `?initialize_monitored_design`")
    } else if(!"monitored_design" %in% class(monitored_design)){
      stop("`monitored_design` must be of class \"monitored_design\": ",
           "See `?initialize_monitored_design`")
    } else if(length(monitored_design) > 1){

      # 1. Check for changes to design
      original_design <- monitored_design$original_design$trial_design
      previous_analyses <- monitored_design[-1]
      k <- length(previous_analyses)
      current_design <- previous_analyses[[k]]$trial_design

      compare_params <-
        c("kMax", "alpha", "beta", "sided",
          "typeOfDesign", "gammaA","typeBetaSpending", "gammaB")

      for(i in 1:k){
        if(
          !identical(
            x = as.list(previous_analyses[[i]]$trial_design)[compare_params],
            y = as.list(original_design)[compare_params]
          )
        ){
          stop("Inconsistencies in `trial_design` parameters between original ",
               "design and analysis ", i , " for one of the following: ",
               paste(compare_params, collapse = ", "))
        }
      }


      # 2. Check previous analyses for stopping boundaries
      previous_decisions <-
        sapply(
          X = previous_analyses,
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


      # 3. Check Consistency of Results
      if(k > 1){

        check_results <-
          c("estimates", "variance")

        previous_results <-
          lapply(
            X = previous_analyses,
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
            stop("Inconsistency in monitored_design element `", i, "`.")
          }

          unique_results <-
            apply(
              X = extracted_result,
              MARGIN = 1,
              FUN = function(x) length(unique(stats::na.omit(object = x)))
            )

          if(any(unique_results > 1)){
            stop("Inconsistency in monitored_design element `", i, "`.")
          }
        }
      }
    }
  }
