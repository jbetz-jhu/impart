#' Determine the information level required for a one-stage, fixed sample size design
#'
#' In information monitored designs, data are accrued until the information
#' (i.e. precision) reaches the level required to achieve the desired power when
#' testing the null hypothesis against a specific alternative while maintaining
#' a specified Type I Error Rate.
#' \code{required_information_uninflated_mann_whitney} is an alias to
#' \code{required_information_single_stage} for ordinal outcomes, where the null
#' value of the Mann-Whitney estimand is 0.5.
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
#' This function allows a user to determine the target level of information
#' \eqn{\mathcal{I}} needed to detect a difference of \eqn{\delta_{min}} in the
#' estimand with power \eqn{(1 - \beta)} while maintaining a type I error rate
#' of \eqn{(\alpha)}.
#'
#' @param alpha Desired Type I Error Rate of the test
#' @param sides (Scalar - 1 or 2): Type of Test, either 1-sided or 2-sided.
#' @param power Desired power of the test (1 - Type II Error Rate)
#'
#' @name required_info
#'
#' @return When all parameters are scalars, the result is a scalar, indicating
#' the required information. When multiple values are specified, a grid of
#' unique parameters are constructed, and the required information is
#' computed for each value of the parameters.
#'
#' @export
#'
#' @seealso [rpact::getDesignGroupSequential] for planning multi-stage
#' designs, and [impart::required_information_sequential] for adjusting the
#' information level from a single stage design to a multi-stage design. To
#' estimate the sample size (or count of events observed) at which the
#' information target may be reached under assumed values of the nuisance
#' parameters, see
#' [impart::asymptotic_information_difference_means],
#' [impart::asymptotic_information_difference_proportions],
#' [impart::asymptotic_information_relative_risk],
#' [impart::asymptotic_information_mann_whitney_fm], and
#' [impart::asymptotic_information_logrank].
#'
#' @references {
#' Mehta, CR, and Tsiatis AA. 2001. "Flexible Sample Size Considerations Using
#' Information-Based Interim Monitoring." \emph{Drug Information Journal}
#' 35 (4): 1095–1112. \url{https://doi.org/10.1177/009286150103500407}
#' }
#'
#' @examples
#' # When a single value is supplied for each parameter, a scalar is returned:
#' required_information_single_stage(
#'   delta = 5,
#'   delta_0 = 0,
#'   alpha = 0.05,
#'   sides = 2,
#'   power = 0.8
#' )
#'
#' # When multiple values are supplied for one or more parameters, the grid of
#' # parameters are created, and a data.frame is returned.
#' required_information_single_stage(
#'   delta = c(5, 7.5),
#'   delta_0 = 0,
#'   alpha = 0.05,
#'   sides = 2,
#'   power = c(0.8, 0.9)
#' )





#' @rdname required_info
#' @param delta Numeric vector containing the estimand under the alternative hypothesis
#' @param delta_0 Numeric vector containing the estimand under the null hypothesis

required_information_single_stage <-
  function(
    delta,
    delta_0 = 0,
    alpha = 0.05,
    sides = 2,
    power = 0.8
  ) {

    if(!(identical(x = sides, y = 1) | identical(x = sides, y = 2))){
      stop("`sides` must be either 1 or 2.")
    }

    if(any(power < 0) | any(power >= 1)){
      stop("`power` must be in the interval (`alpha` = ", alpha, ", 1)")
    }

    if(any(alpha < 0) | any(alpha >= 1)){
      stop("`alpha` must be in the interval (0, 1)")
    }

    param_grid <-
      expand.grid(
        delta = delta,
        delta_0 = delta_0,
        alpha = alpha,
        sides = sides,
        power = power
      )

    param_grid <- param_grid[which(!duplicated(param_grid)),]

    if(any(with(data = param_grid, power < alpha))){
      stop("`power` must be in the interval (`alpha` = ", alpha, ", 1)")
    }

    param_grid$information <-
      with(
        data = param_grid,
        ((qnorm(p = 1 - (alpha/sides)) + qnorm(p = power))/(delta - delta_0))^2
      )

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(param_grid$information)
    }
  }




#' @rdname required_info
#' @param mw Numeric vector containing the estimand under the alternative
#' hypothesis: the value of the Mann-Whitney under the null is always 0.5.

required_information_mw_single_stage <-
  function(
    mw,
    alpha = 0.05,
    sides = 2,
    power = 0.8
  ) {
    return(
      required_information_single_stage(
        delta = mw,
        delta_0 = 0.5,
        sides = sides,
        power = power
      )
    )
  }
