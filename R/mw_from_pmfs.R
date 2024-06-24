#' Title Compute the Mann-Whitney estimand from Probability Mass Functions
#'
#' The Mann-Whitney estimand is the tie-adjusted probability of a randomly
#' selected individual from the population of treated individuals having a
#' an outcome with a higher outcome category level than a randomly selected
#' individual from the population of control individuals.
#'
#' @param pmf_0 A vector containing the PMF of outcomes in the control
#' population, i.e. Pr(Y = 1 | A = 0), Pr(Y = 2 | A = 0), ...
#' @param pmf_1 A vector containing the PMF of outcomes in the treated
#' population, i.e. Pr(Y = 1 | A = 1), Pr(Y = 2 | A = 1), ...
#' @param reverse_scale (Scalar: Logical) Should the scales be reversed when
#' calculating the Mann-Whitney estimand? This may be useful when lower
#' categories indicate a preferable outcome.
#'
#' @return A scalar containing the Mann-Whitney Estimand
#'
#' @export
#'
#' @examples
#' mw_from_pmfs(
#'   pmf_0 = c(0.1, 0.1, 0.2, 0.6),
#'   pmf_1 = c(0.2, 0.2, 0.3, 0.3)
#' )
#'
#' mw_from_pmfs(
#'   pmf_0 = c(0.1, 0.1, 0.2, 0.6),
#'   pmf_1 = c(0.2, 0.2, 0.3, 0.3),
#'   reverse_scale = TRUE
#' )

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
