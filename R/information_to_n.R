#' Convert information level into an approximate sample size requirement for a
#' one-stage design
#'
#' These functions provide an asymptotic approximation to the information
#' (i.e. precision, inverse of the variance) provided by two samples under
#' assumed values of nuisance parameters for continuous and binary outcomes.
#'
#' @param information Numeric vector containing the information level
#' @param round_up Logical scalar: should the sample size be rounded up to an
#' integer value?
#'
#' @name information_to_n
#'
#' @return When all parameters are scalars, the result is a scalar, indicating
#' the approximate sample size requirement. When multiple values are specified,
#' a grid of unique parameters are constructed, and the approximate information
#' is computed for each value of the parameters.
#'
#' @export
#'
#' @seealso [impart::asymptotic_information_difference_means()],
#' [impart::asymptotic_information_difference_proportions()], and
#' [impart::asymptotic_information_mann_whitney_fm()] for an asymptotic
#' approximation of the information for a given sample size and values of the
#' nuisance parameters.
#'
#' @examples
#' # To add
#'


#' @rdname information_to_n
#' @param sigma_0 Variance of outcomes in the population of individuals
#' receiving the control intervention
#' @param sigma_1 Variance of outcomes in the population of individuals
#' receiving the active intervention

information_to_n_continuous_1_to_1 <-
  function(
    information,
    sigma_0,
    sigma_1,
    round_up = TRUE
  ) {

    if(any(information <= 0)){
      stop("All elements of `information` must be positive.")
    }

    if(any(sigma_0 <= 0) | any(sigma_1 <= 0)){
      stop("All elements of `sigma_0` and `sigma_1` must be positive.")
    }

    param_grid <-
      expand.grid(
        information = information,
        sigma_0 = sigma_0,
        sigma_1 = sigma_1
      )

    param_grid <- param_grid[which(!duplicated(param_grid)),]

    param_grid$n_per_arm <-
      with(
        data = param_grid,
        expr = information*((sigma_0^2) + (sigma_1^2))
      )

    if(round_up) {
      param_grid$n_per_arm <- ceiling(param_grid$n_per_arm)
    }

    param_grid$n_total <- 2*param_grid$n_per_arm

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(
        data.frame(
          "n_per_arm" = param_grid$n_per_arm,
          "n_total" = param_grid$n_total
        )
      )
    }
  }




#' @rdname information_to_n
#' @param pi_0 Probability of event in the population of individuals
#' receiving the control intervention
#' @param pi_1 Probability of event in the population of individuals
#' receiving the control intervention
#' @param delta The risk difference (i.e. \code{pi_1 - pi_0})
information_to_n_binary_1_to_1 <-
  function(
    information,
    pi_0 = NULL,
    pi_1 = NULL,
    delta = NULL,
    round_up = TRUE
  ) {

    if(any(information <= 0)){
      stop("All elements of `information` must be positive.")
    }

    if(is.null(pi_0) + is.null(pi_1) + is.null(delta) != 1){
      stop("Only two of the following parameters should be specified: `pi_0`, ",
           "`pi_1`, `delta`")
    }

    if(is.null(pi_0)) {
      param_grid <-
        expand.grid(
          information = information,
          pi_1 = pi_1,
          delta = delta
        )

      param_grid$pi_0 <-
        with(
          data = param_grid,
          expr = pi_1 - delta
        )
    } else if(is.null(pi_1)) {
      param_grid <-
        expand.grid(
          information = information,
          pi_0 = pi_0,
          delta = delta
        )

      param_grid$pi_1 <-
        with(
          data = param_grid,
          expr = pi_0 + delta
        )
    } else if(is.null(delta)) {
      param_grid <-
        expand.grid(
          information = information,
          pi_0 = pi_0,
          pi_1 = pi_1
        )
    }

    if(any(param_grid$pi_0 <= 0) | any(param_grid$pi_0 >= 1)){
      stop("`pi_0` (i.e. `pi_1 - delta`) should be in the interval (0, 1).")
    }

    if(any(param_grid$pi_1 <= 0) | any(param_grid$pi_1 >= 1)){
      stop("`pi_1` (i.e. `pi_0 + delta`) should be in the interval (0, 1).")
    }

    param_grid <- param_grid[which(!duplicated(param_grid)),]

    param_grid$n_per_arm <-
      with(
        data = param_grid,
        expr =
          2*information*((pi_0*(1 - pi_0)) + (pi_1*(1 - pi_1)))/2
      )

    if(round_up) {
      param_grid$n_per_arm <- ceiling(param_grid$n_per_arm)
    }

    param_grid$n_total <- 2*param_grid$n_per_arm

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(
        data.frame(
          "n_per_arm" = param_grid$n_per_arm,
          "n_total" = param_grid$n_total
        )
      )
    }
  }

