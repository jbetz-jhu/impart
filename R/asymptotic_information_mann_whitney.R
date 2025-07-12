#' Compute Approximate Information from Sample Size: Mann-Whitney Estimand
#'
#' This function provides an asymptotic approximation to the information
#' (i.e. precision, inverse of the variance) provided by two samples under
#' assumed values of nuisance parameters for an ordinal outcome, analyzed using
#' the Mann-Whitney estimand. This can be obtained by specifying the value of
#' the Mann-Whitney estimand or the probability mass functions of outcomes in
#' each treatment arm. These functions may be useful in pre-trial
#' planning to determine when analyses may occur under different assumptions
#' about the nuisance parameters involved.
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
#' @param mw A \code{numeric} vector containing the Mann-Whitney estimand.
#' @param pmf_0 A \code{numeric} vector or matrix of row vectors, each
#' containing the probability mass function of outcomes in the population of
#' individuals receiving the control intervention.
#' @param pmf_1 A \code{numeric} vector or matrix of row vectors, each
#' containing the probability mass function of outcomes in the population of
#' individuals receiving the active intervention.
#' @param adjust A \code{logical} scalar, indicating whether an adjustment for
#' ties should be performed. \emph{Note}: this can only be computed when
#' \code{pmf_0} and \code{pmf_1} are supplied.
#'
#' @name mann_whitney
#'
#' @return When all parameters are scalars, the result is a scalar, indicating
#' the approximate information. When multiple values are specified, a grid of
#' unique parameters are constructed, and the approximate information is
#' computed for each value of the parameters.
#'
#' @export
#'
#' @seealso [asymptotic_information_difference_means] for the
#' information on the difference in means,
#' [asymptotic_information_difference_proportions] for the information
#' on a difference in proportions (i.e. a risk difference),
#' [asymptotic_information_relative_risk] for the information on the
#' relative risk (i.e. risk ratio), and [asymptotic_information_logrank]
#' for information on the log hazard ratio estimand.
#'
#' @references {
#' Fay, MP and Malinovsky, Y. 2018. "Confidence Intervals of the Mann-Whitney
#' Parameter That Are Compatible with the Wilcoxon-Mann-Whitney Test."
#' \emph{Statistics in Medicine} 37 (27): 3991–4006.
#' \url{https://doi.org/10.1002/sim.7890}.
#'
#' Zhao, YD, Rahardja D, and Qu Y. 2007. "Sample Size Calculation for the
#' Wilcoxon–Mann–Whitney Test Adjusting for Ties." \emph{Statistics in Medicine}
#' 27 (3): 462–68. https://doi.org/10.1002/sim.2912.
#'
#' Benkeser, D, Díaz, I, Luedtke, A, Segal, J, Scharfstein, D, and Rosenblum,
#' M. 2020. "Improving Precision and Power in Randomized Trials for COVID-19
#' Treatments Using Covariate Adjustment, for Binary, Ordinal, and Time-to-Event
#' Outcomes." \emph{Biometrics} 77 (4): 1467–81.
#' \url{https://doi.org/10.1111/biom.13377}.
#' }
#'
#' @examples
#' # When a single value is supplied for each parameter, a scalar is returned:
#' asymptotic_information_mann_whitney_fm(
#'     n_0 = 100,
#'     n_1 = 100,
#'     mw = 0.75,
#'     adjust = FALSE
#'   )
#'
#' # When multiple values are supplied for one or more parameters, the grid of
#' # parameters are created, and a data.frame is returned.
#' asymptotic_information_mann_whitney_fm(
#'   n_0 = c(100, 150),
#'   n_1 = c(100, 150),
#'   mw = 0.75,
#'   adjust = FALSE
#' )
#'
#'
#' # Specifying PMFs - With and Without Tie Adjustment
#' asymptotic_information_mann_whitney_fm(
#'   n_0 = 100,
#'   n_1 = 100,
#'   pmf_0 = c(0.2, 0.2, 0.6),
#'   pmf_1 = c(0.1, 0.1, 0.8),
#'   adjust = TRUE
#' )
#'
#' # Specifying Multiple PMFs
#' asymptotic_information_mann_whitney_fm(
#'   n_0 = 100,
#'   n_1 = 100,
#'   pmf_0 =
#'     rbind(
#'       c(0.2, 0.2, 0.6),
#'       c(0.3, 0.1, 0.6)
#'     ),
#'   pmf_1 =
#'     rbind(
#'       c(0.1, 0.1, 0.8),
#'       c(0.05, 0.05, 0.9)
#'       ),
#'   adjust = TRUE
#' )

#' @rdname mann_whitney
asymptotic_information_mann_whitney_fm <-
  function(
    n_0,
    n_1,
    mw = NULL,
    pmf_1 = NULL,
    pmf_0 = NULL,
    adjust = TRUE
  ) {

    if(all(!is.null(mw), !is.null(pmf_0), !is.null(pmf_1))){
      stop("Only (`pmf_0` and `pmf_1`) or `mw` should be specified. ")
    }

    if(any(n_0 < 1) | any(n_1 < 1)){
      stop("All elements of `n_0` and `n_1` must be greater than 1.")
    }

    if(adjust & (is.null(pmf_0) | is.null(pmf_1))){
      stop("Adjustment for ties requires specifying `pmf_0` and `pmf_1`")
    }

    if(is.vector(pmf_0)){
      if(sum(pmf_0) != 1){
        stop("`pmf_0` does not sum to 1.")
      } else{
        n_pmf_0 <- 1
        levels_pmf_0 <- length(pmf_0)
        pmf_0 <- list(pmf_0 = pmf_0)
      }
    } else if(is.matrix(pmf_0)){
      if(!all(apply(X = pmf_0, MARGIN = 1, FUN = sum) == 1)){
        stop("Each row of `pmf_0` should sum to 1.")
      } else {
        n_pmf_0 <- nrow(pmf_0)
        levels_pmf_0 <- ncol(pmf_0)
        pmf_0 <- as.list(data.frame(t(pmf_0)))
      }
    }

    if(is.vector(pmf_1)){
      if(sum(pmf_1) != 1){
        stop("`pmf_1` does not sum to 1.")
      } else{
        n_pmf_1 <- 1
        levels_pmf_1 <- length(pmf_1)
        pmf_1 <- list(pmf_1 = pmf_1)
      }
    } else if(is.matrix(pmf_1)){
      if(!all(apply(X = pmf_1, MARGIN = 1, FUN = sum) == 1)){
        stop("Each row of `pmf_1` should sum to 1.")
      } else {
        n_pmf_1 <- nrow(pmf_1)
        levels_pmf_1 <- ncol(pmf_1)
        pmf_1 <- as.list(data.frame(t(pmf_1)))
      }
    }


    if(is.null(mw)){
      if(is.null(pmf_1) & is.null(pmf_0)){
        stop("If `mw` is NULL then `pmf_1` and `pmf_0` must be supplied.")
      } else if(levels_pmf_0 != levels_pmf_1){
        stop("Number of levels in `pmf_0` (", levels_pmf_0, ") and `pmf_1` (",
             levels_pmf_1, ") are not equal.")
      } else {
        param_grid <-
          expand.grid(
            n_0 = n_0,
            n_1 = n_1,
            pmf_0 = 1:n_pmf_0,
            pmf_1 = 1:n_pmf_1,
            mw = NA,
            t = 1
          )

        param_grid[
          c(paste0("pmf_0_", 1:levels_pmf_0), paste0("pmf_1_", 1:levels_pmf_1))
        ] <- NA

        for(i in 1:nrow(param_grid)){
          p0 <- pmf_0[[param_grid$pmf_0[i]]]
          p1 <- pmf_1[[param_grid$pmf_1[i]]]

          param_grid[i, c(paste0("pmf_0_", 1:levels_pmf_0))] <- p0
          param_grid[i, c(paste0("pmf_1_", 1:levels_pmf_1))] <- p1

          param_grid$mw[i] <-
            mw_from_pmfs(
              pmf_0 = p0,
              pmf_1 = p1
            )

          if(adjust){
            n <- param_grid$n_0[i] + param_grid$n_1[i]
            ties <- param_grid$n_0[i]*p0 + param_grid$n_1[i]*p1
            param_grid$t[i] <- 1 - sum(ties^3 - ties)/(n^3 - n)
          }
        }
      }
    } else {
      param_grid <-
        expand.grid(
          n_0 = n_0,
          n_1 = n_1,
          mw = mw,
          t = 1
        )
    }

    param_grid$information_asymptotic <-
      with(
        data = param_grid,
        expr = t*(mw*(1 - mw)/(n_0*n_1))*
          (1 + (1/2)*(n_0 + n_1 - 2)*((mw/(1 + mw)) + ((1 - mw)/(2 - mw))))
      )

    if(nrow(param_grid) > 1){
      return(param_grid)
    } else{
      return(param_grid$information_asymptotic)
    }
  }




#' @rdname mann_whitney
#' @param reverse_scale A \code{logical} scalar: should the scales be reversed
#' when calculating the Mann-Whitney estimand? This may be useful when lower
#' categories indicate a preferable outcome.

mw_from_pmfs <-
  function(
    pmf_0,
    pmf_1,
    reverse_scale = FALSE
  ){
    if(sum(pmf_0) != 1){
      stop("`pmf_0` does not sum to 1.")
    } else if(sum(pmf_1) != 1){
      stop("`pmf_1` does not sum to 1.")
    } else if(any(pmf_0 <= 0)| any(pmf_0 >= 1)){
      stop("All elements of `pmf_0` must be in (0, 1).")
    } else if(any(pmf_1 <= 0)| any(pmf_1 >= 1)){
      stop("All elements of `pmf_0` must be in (0, 1).")
    } else if(length(pmf_0) != length(pmf_1)){
      stop("Length of `pmf_0` and `pmf_1` must be identical.")
    }

    joint_pmf_0_1 <- kronecker(X = pmf_0, Y = t(pmf_1))

    mw <-
      sum(joint_pmf_0_1[upper.tri(x = joint_pmf_0_1, diag = FALSE)]) +
      0.5*sum(diag(joint_pmf_0_1))

    return(
      ifelse(test = reverse_scale, yes = 1 - mw, no = mw)
    )
  }
