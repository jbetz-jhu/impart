#' Convert Information Target into Sample Size for Fixed N Single-Stage Design
#'
#' These functions convert an information target into an approximate sample size
#' required for a single-stage fixed sample size design with the same power and
#' type I error requirements under assumed values of nuisance parameters.
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
#' These functions allow a user to determine an approximate sample size \eqn{N}
#' at which the information target \eqn{\mathcal{I}} would be reached under some
#' assumptions about the nuisance parameters.
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
#' @seealso [impart::asymptotic_information_difference_means],
#' [asymptotic_information_difference_proportions],
#' [asymptotic_information_relative_risk],
#' [asymptotic_information_mann_whitney_fm], and
#' [asymptotic_information_logrank] for an asymptotic
#' approximation of the information for a given sample size and values of the
#' nuisance parameters, and [required_information_single_stage] for
#' determining the information target.
#'
#' @references {
#' Mehta, CR, and Tsiatis AA. 2001. "Flexible Sample Size Considerations Using
#' Information-Based Interim Monitoring". \emph{Drug Information Journal}
#' 35 (4): 1095–1112. \url{https://doi.org/10.1177/009286150103500407}
#' }
#'
#' @examples
#' information_to_n_difference_means(
#'     information =
#'       required_information_single_stage(
#'         delta = 5,
#'         power = 0.8,
#'         alpha = 0.05,
#'         sides = 2
#'       ),
#'     sigma_0 = 10,
#'     sigma_1 = 10,
#'     round_up = TRUE
#'   )
#'
#' power.t.test(
#'   delta = 5, sd = 10, sig.level = 0.05, power = 0.8
#' )
#'
#' information_to_n_risk_difference(
#'   information =
#'     required_information_single_stage(
#'       delta = 0.1,
#'       power = 0.8,
#'       alpha = 0.05,
#'       sides = 2
#'     ),
#'   pi_0 = 0.25,
#'   pi_1 = 0.15,
#'   round_up = TRUE
#' )
#'
#' power.prop.test(p1 = 0.25, p2 = 0.15, sig.level = 0.05, power = 0.8)
#'
#' information_to_n_relative_risk(
#'   information =
#'     required_information_single_stage(
#'       delta = log(0.5),
#'       power = 0.8,
#'       alpha = 0.05,
#'       sides = 2
#'     ),
#'   pi_0 = 0.2,
#'   pi_1 = 0.1,
#'   round_up = TRUE
#' )
#'
#' rr_design(
#'   pi_1 = 0.1,
#'   pi_0 = 0.2,
#'   power = 0.8,
#'   alpha = 0.05
#' )
#'
#' information_to_events_log_hr(
#'   information =
#'     required_information_single_stage(
#'       delta = log(0.5),
#'       power = 0.8,
#'       alpha = 0.05,
#'       sides = 2
#'     ),
#'   allocation_ratio = 1,
#'   round_up = TRUE
#' )
#'
#' hr_design(
#'   hazard_ratio = 0.5,
#'   power = 0.8,
#'   alpha = 0.05,
#'   test_sides = 2
#' )

#' @rdname information_to_n
#' @param sigma_0 Variance of outcomes in the population of individuals
#' receiving the control intervention
#' @param sigma_1 Variance of outcomes in the population of individuals
#' receiving the active intervention
#' @export
information_to_n_difference_means <-
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
#' @export
information_to_n_risk_difference <-
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




#' @rdname information_to_n
#' @param pi_0 Probability of event in the population of individuals
#' receiving the control intervention
#' @param pi_1 Probability of event in the population of individuals
#' receiving the control intervention
#' @param rr The relative risk of the event in treatment vs. control (i.e. \code{pi_1/pi_0})
#' @export
information_to_n_relative_risk <-
  function(
    information,
    pi_0 = NULL,
    pi_1 = NULL,
    rr = NULL,
    round_up = TRUE
  ) {

    if(any(information <= 0)){
      stop("All elements of `information` must be positive.")
    }

    if(is.null(pi_0) + is.null(pi_1) + is.null(rr) != 1){
      stop("Only two of the following parameters should be specified: `pi_0`, ",
           "`pi_1`, `rr`")
    }

    if(is.null(pi_0)) {
      param_grid <-
        expand.grid(
          information = information,
          pi_1 = pi_1,
          rr = rr
        )

      param_grid$pi_0 <-
        with(
          data = param_grid,
          expr = pi_1/rr
        )
    } else if(is.null(pi_1)) {
      param_grid <-
        expand.grid(
          information = information,
          pi_0 = pi_0,
          rr = rr
        )

      param_grid$pi_1 <-
        with(
          data = param_grid,
          expr = pi_0*rr
        )
    } else if(is.null(rr)) {
      param_grid <-
        expand.grid(
          information = information,
          pi_0 = pi_0,
          pi_1 = pi_1
        )

      param_grid$rr <-
        with(
          data = param_grid,
          expr = pi_1/pi_0
        )
    }

    if(any(param_grid$pi_0 <= 0) | any(param_grid$pi_0 >= 1)){
      stop("`pi_0` (i.e. `pi_1/rr`) should be in the interval (0, 1).")
    }

    if(any(param_grid$pi_1 <= 0) | any(param_grid$pi_1 >= 1)){
      stop("`pi_1` (i.e. `pi_0*rr`) should be in the interval (0, 1).")
    }

    param_grid <- param_grid[which(!duplicated(param_grid)),]

    param_grid$n_per_arm <-
      with(
        data = param_grid,
        expr =
          information*((1 - rr*pi_0)/(rr*pi_0) + (1 - pi_0)/(pi_0))
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
#' @param allocation_ratio The allocation ratio of participants receiving
#' treatment to those receiving control
#' @export
information_to_events_log_hr <-
  function(
    information,
    allocation_ratio = 1,
    round_up = TRUE
  ) {

    if(any(information <= 0)){
      stop("All elements of `information` must be positive.")
    }

    if(any(!is.finite(allocation_ratio))){
      stop("All elements of `allocation_ratio` must be numeric and greater than 0.")
    } else if(any(allocation_ratio <= 0)){
      stop("All elements of `allocation_ratio` must be greater than 0.")
    }

    param_grid <-
      expand.grid(
        information = information,
        allocation_ratio = allocation_ratio
      )

    param_grid <- param_grid[which(!duplicated(param_grid)),]

    param_grid$total_events <-
      with(
        data = param_grid,
        expr =
          (information*(1 + allocation_ratio)^2)/allocation_ratio
      )

    if(round_up) {
      param_grid$total_events <- ceiling(param_grid$total_events)
    }

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(
        param_grid$total_events
      )
    }
  }
