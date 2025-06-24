#' Approximate information from a random samples of a given size for log hazard ratio
#'
#' @param allocation_ratio Numeric scalar containing the allocation ratio of r
#' treatments to 1 control. Defaults to 1.
#' @param total_events Numeric vector containing the total number of events
#' observed across both treatment arms
#'
#' @returns A \code{numeric} scalar or \code{data.frame} containing an
#' approximate information level for the values of the inputs.
#'
#' @export
#'
#' @examples
#' asymptotic_information_logrank(
#'   allocation_ratio = 1,
#'   total_events = 90
#' )
#'
#' asymptotic_information_logrank(
#'   allocation_ratio = 1,
#'   total_events = c(66, 90)
#' )
#'
#' asymptotic_information_logrank(
#'   allocation_ratio = c(1, 2),
#'   total_events = c(66, 90)
#' )

asymptotic_information_logrank <-
  function(
    allocation_ratio = 1,
    total_events
  ) {

    if(any(!is.finite(allocation_ratio))){
      stop("All elements of `allocation_ratio` must be numeric and greater than 0.")
    } else if(any(allocation_ratio <= 0)){
      stop("All elements of `allocation_ratio` must be greater than 0.")
    }

    if(any(total_events < 1)){
      stop("`total_events` must be greater than 1.")
    }

    param_grid <-
      expand.grid(
        allocation_ratio = allocation_ratio,
        total_events = total_events,
        information_asymptotic = NA
      )

    param_grid <- param_grid[which(!duplicated(param_grid)),]

    param_grid$information_asymptotic <-
      with(data = param_grid,
           expr = {
             total_events*(allocation_ratio)/(1 + allocation_ratio)^2
           }
      )

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(param_grid$information_asymptotic)
    }
  }
