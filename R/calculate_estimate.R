#' Compute an estimate from a wrapper function
#'
#' @param data A data.frame containing the data to be analyzed
#' @param estimation_function A function whose arguments include a data.frame
#' named \code{data}
#' @param estimation_arguments A list of any additional arguments needed by
#' \code{estimation_function}
#'
#' @return A numeric scalar containing the computed estimate
#'
#' @export
#'
#' @seealso [impart::calculate_covariance()] for computing the covariance matrix
#' of estimators across analyses.
#'
#' @examples
#' # To be added

calculate_estimate <-
  function(
    data,
    estimation_function,
    estimation_arguments
  ){
    return(
      get(
        x = "estimate",
        pos =
          do.call(
            what = estimation_function,
            args = c(list(data = data), estimation_arguments)
          )
      )
    )
  }
