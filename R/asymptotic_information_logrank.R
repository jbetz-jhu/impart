#' Compute Approximate Information from Event Counts: Log Hazard Ratio
#'
#' This function provides an asymptotic approximation to the information
#' (i.e. precision, inverse of the variance) provided by a number of observed
#' events pooled across treatment arms for a time-to-event outcome, analyzed
#' using the log hazard ratio estimand. These functions may be useful in
#' pre-trial planning to determine when analyses may occur under different
#' assumptions about the nuisance parameters involved.
#'
#' @param allocation_ratio A \code{numeric} scalar containing the allocation
#' ratio of r participants to treatments for every 1 control. Defaults to 1.
#' @param total_events A \code{numeric} vector containing the total number of
#' events observed across both treatment arms.
#'
#' @returns A \code{numeric} scalar or \code{data.frame} containing an
#' approximate information level for the values of the inputs.
#'
#' @export
#'
#' @seealso [impart::asymptotic_information_difference_means] for the
#' information on the difference in means,
#' [impart::asymptotic_information_difference_proportions] for the information
#' on a difference in proportions (i.e. a risk difference),
#' [impart::asymptotic_information_relative_risk] for the information on the
#' relative risk (i.e. risk ratio), and
#' [impart::asymptotic_information_mann_whitney_fm] for information on the
#' Mann-Whitney estimand.
#'
#' @references {
#' Schoenfeld, DA. 1983. "Sample-Size Formula for the Proportional-Hazards
#' Regression Model." \emph{Biometrics} 39 (2): 499.
#' \url{https://doi.org/10.2307/2531021}.
#'
#' Mehta, CR, and Tsiatis AA. 2001. "Flexible Sample Size Considerations Using
#' Information-Based Interim Monitoring". \emph{Drug Information Journal}
#' 35 (4): 1095–1112. \url{https://doi.org/10.1177/009286150103500407}
#' }
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
