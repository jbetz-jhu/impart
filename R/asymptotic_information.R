#' Approximate information from a random samples of a given size for continuous and binary outcomes
#'
#' These functions provide an asymptotic approximation to the information
#' (i.e. precision, inverse of the variance) provided by two samples under
#' assumed values of nuisance parameters for continuous and binary outcomes.
#'
#' @param n_0 Numeric vector containing the sample size in the control arm
#' @param n_1 Numeric vector containing the sample size in the treatment arm
#'
#' @name asymptotic_info
#'
#' @return When all parameters are scalars, the result is a scalar, indicating
#' the approximate information. When multiple values are specified, a grid of
#' unique parameters are constructed, and the approximate information is
#' computed for each value of the parameters.
#'
#' @export
#'
#' @seealso [asymptotic_information_mann_whitney_fm] for ordinal outcomes.
#'
#' @examples
#' # When a single value is supplied for each parameter, a scalar is returned:
#' asymptotic_information_difference_means(
#'   n_0 = 50,
#'   sigma_0 = 5,
#'   n_1 = 50,
#'   sigma_1 = 5
#' )
#'
#' # When multiple values are supplied for one or more parameters, the grid of
#' # parameters are created, and a data.frame is returned.
#' asymptotic_information_difference_means(
#'   n_0 = c(50, 75),
#'   sigma_0 = 5,
#'   n_1 = c(50, 75),
#'   sigma_1 = 5
#' )




#' @rdname asymptotic_info
#' @param sigma_0 Variance of outcomes in the population of individuals
#' receiving the control intervention
#' @param sigma_1 Variance of outcomes in the population of individuals
#' receiving the active intervention

asymptotic_information_difference_means <-
  function(
    n_0,
    sigma_0,
    n_1,
    sigma_1
  ) {
    if(any(n_0 < 1) | any(n_1 < 1)){
      stop("All elements of `n_0` and `n_1` must be greater than 1.")
    }

    if(any(sigma_0 <= 0) | any(sigma_1 <= 0)){
      stop("All elements of `sigma_0` and `sigma_1` must be positive.")
    }

    param_grid <-
      expand.grid(
        n_0 = n_0,
        sigma_0 = sigma_0,
        n_1 = n_1,
        sigma_1 = sigma_1
      )

    param_grid <- param_grid[which(!duplicated(param_grid)),]

    param_grid$information_asymptotic <-
      with(
        data = param_grid,
        expr = 1/((sigma_0^2/n_0) + (sigma_1^2/n_1))
      )

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(param_grid$information_asymptotic)
    }
  }




#' @rdname asymptotic_info
#' @param pi_0 Probability of event in the population of individuals
#' receiving the control intervention
#' @param pi_1 Probability of event in the population of individuals
#' receiving the control intervention

asymptotic_information_difference_proportions <-
  function(
    n_0,
    pi_0,
    n_1,
    pi_1
  ) {
    if(any(n_0 < 1) | any(n_1 < 1)){
      stop("All elements of `n_0` and `n_1` must be greater than 1.")
    }

    if(any(pi_0 <= 0) | any(pi_1 <= 0)){
      stop("All elements of `pi_0` and `pi_1` must be positive.")
    }

    param_grid <-
      expand.grid(
        n_0 = n_0,
        pi_0 = pi_0,
        n_1 = n_1,
        pi_1 = pi_1
      )

    param_grid <- param_grid[which(!duplicated(param_grid)),]

    param_grid$information_asymptotic <-
      with(
        data = param_grid,
        expr = 1/((1/n_0)*pi_0*(1 - pi_0) + (1/n_1)*pi_1*(1 - pi_1))
      )

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(param_grid$information_asymptotic)
    }
  }
