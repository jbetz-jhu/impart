#' Compute Approximate Information from Sample Size: Continuous & Binary Outcomes
#'
#' These functions provide an asymptotic approximation to the information
#' (i.e. precision, inverse of the variance) provided by two samples under
#' assumed values of nuisance parameters for continuous and binary outcomes.
#' This includes a difference in means for continuous outcomes, a difference in
#' proportions (i.e. risk difference) for a binary outcome, or a relative risk
#' (i.e. risk ratio) for a binary outcome. These functions may be useful in
#' pre-trial planning to determine when analyses may occur under different
#' assumptions about the nuisance parameters involved.
#'
#' The amount of information in a sample of size \eqn{N} depends on
#' \emph{nuisance parameters}, such as the variance of continuous outcomes,
#' the risk of binary and time-to-event outcomes, rates of misssing data, and
#' the correlation between covariates and the outcomes of interest.
#'
#' In studies with a fixed sample size, this sample size is chosen based on
#' assumptions about these nuisance parameters, which are incorporated into the
#' \emph{effect size}. The sample size is chosen to give power \eqn{(1 - \beta)}
#' while maintaining a type I error rate of \eqn{(\alpha)} under some assumed
#' effect size. Inaccurate estimates of nuisance parameters can lead to
#' over-powered or under-powered studies.
#'
#' In an information-monitored design, investigators choose an estimand of
#' interest, such as the difference in means or proportions, that is free from
#' nuisance parameters. A trial is designed to identify some minimum important
#' difference \eqn{\delta_{min}} in the estimand with power \eqn{(1 - \beta)}
#' while maintaining a type I error rate of \eqn{(\alpha)}. Data is collected
#' until the precision of the estimate (i.e. the reciprocal of its variance)
#' reaches a pre-specified threshold \eqn{\mathcal{I}}:
#'
#' \deqn{\mathcal{I} = \left(\frac{Z_{\alpha/s} + Z_{\beta}}{\delta_{min}}\right)^2
#' \approx \frac{1}{Var(\hat{\delta})} =
#' \frac{1}{\left(SE(\hat{\delta})\right)^2}}
#'
#' The sample size required to reach the information target \eqn{\mathcal{I}}
#' depends on nuisance parameters mentioned above.
#'
#' These functions allow a user to determine an approximate amount of
#' information contained in a sample of size \eqn{N} based on some assumptions
#' about the nuisance parameters.
#'
#' @param n_0 A \code{numeric} vector containing the sample size in the control
#' arm.
#' @param n_1 A \code{numeric} vector containing the sample size in the
#' treatment arm.
#'
#' @name asymptotic_information
#'
#' @return When all parameters are scalars, the result is a scalar, indicating
#' the approximate information. When multiple values are specified, a grid of
#' unique parameters are constructed, and the approximate information is
#' computed for each value of the parameters.
#'
#' @export
#'
#' @seealso [asymptotic_information_mann_whitney_fm] estimates information
#' from a sample of size N using the Mann-Whitney estimand, and
#' [asymptotic_information_logrank] estimates information from a number
#' of events observed using the log hazard ratio estimand. The functions
#' [information_to_n_difference_means],
#' [information_to_n_risk_difference],
#' [information_to_n_relative_risk], and
#' [information_to_events_log_hr] convert information targets into a
#' sample size (or number of observed events required) based on assumed values
#' for the nuisance parameters.
#'
#' @references {
#' Mehta, CR, and Tsiatis AA. 2001. "Flexible Sample Size Considerations Using
#' Information-Based Interim Monitoring." \emph{Drug Information Journal}
#' 35 (4): 1095–1112. \url{https://doi.org/10.1177/009286150103500407}
#'
#' Mehta CR, Gao P, Bhatt DL, Harrington RA, Skerjanec S, and Ware JH. 2009.
#' "Optimizing Trial Design: Sequential, Adaptive, and Enrichment Strategies."
#' \emph{Circulation} 119 (4): 597–605.
#' \url{https://doi.org/10.1161/circulationaha.108.809707}.
#' }
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
#' asymptotic_information_difference_proportions(
#'   n_0 = 20,
#'   pi_0 = 0.2,
#'   n_1 = 20,
#'   pi_1 = 0.1
#' )
#'
#' asymptotic_information_relative_risk(
#'   n_0 = 20,
#'   pi_0 = 0.2,
#'   n_1 = 20,
#'   pi_1 = 0.1
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
#'
#' asymptotic_information_difference_proportions(
#'   n_0 = c(20, 40),
#'   pi_0 = 0.2,
#'   n_1 = c(20, 40),
#'   pi_1 = 0.1
#' )
#'
#' asymptotic_information_relative_risk(
#'   n_0 = c(20, 40),
#'   pi_0 = 0.2,
#'   n_1 = c(20, 40),
#'   pi_1 = 0.1
#' )


#' @rdname asymptotic_information
#' @param sigma_0 Variance of outcomes in the population of individuals
#' receiving the control intervention
#' @param sigma_1 Variance of outcomes in the population of individuals
#' receiving the active intervention
#' @export

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




#' @rdname asymptotic_information
#' @param pi_0 Probability of event in the population of individuals
#' receiving the control intervention
#' @param pi_1 Probability of event in the population of individuals
#' receiving the control intervention
#' @export

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

    if(any(pi_0 >= 1) | any(pi_1 >= 1)){
      stop("All elements of `pi_0` and `pi_1` must be less than 1.")
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


#' @rdname asymptotic_information
#' @param ... Arguments passed to [asymptotic_information_difference_proportions]
#' @export

asymptotic_information_risk_difference <-
  function(...){
    asymptotic_information_difference_proportions(...)
  }

#' @rdname asymptotic_information
#' @param pi_0 Probability of event in the population of individuals
#' receiving the control intervention
#' @param pi_1 Probability of event in the population of individuals
#' receiving the control intervention
#' @export

asymptotic_information_relative_risk <-
  function(
    n_0,
    pi_0,
    n_1,
    pi_1
  ){
    if(any(n_0 < 1) | any(n_1 < 1)){
      stop("All elements of `n_0` and `n_1` must be greater than 1.")
    }

    if(any(pi_0 <= 0) | any(pi_1 <= 0)){
      stop("All elements of `pi_0` and `pi_1` must be positive.")
    }

    if(any(pi_0 >= 1) | any(pi_1 >= 1)){
      stop("All elements of `pi_0` and `pi_1` must be less than 1.")
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
        expr = 1/(1/(n_0*pi_0) - 1/(n_0) + 1/(n_1*pi_1) - 1/(n_1))
      )

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(param_grid$information_asymptotic)
    }
  }
