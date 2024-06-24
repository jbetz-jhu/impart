#' Approximate information from a random samples of a given size for ordinal
#' outcomes
#'
#' These functions provide an asymptotic approximation to the information
#' (i.e. precision, inverse of the variance) provided by two samples under
#' either an assumed value of the Mann-Whitney estimand or probability mass
#' functions (PMFs) of an ordinal outcome in treatment and control populations.
#'
#' @param n_0 Numeric vector containing the sample size in the control arm
#' @param n_1 Numeric vector containing the sample size in the treatment arm
#' @param mw Numeric vector containing the Mann-Whitney estimand
#' @param pmf_0 Numeric vector or matrix of row vectors, each containing the
#' probability mass function of outcomes in the population of individuals
#' receiving the control intervention
#' @param pmf_1 Numeric vector or matrix of row vectors, each containing the
#' probability mass function of outcomes in the population of individuals
#' receiving the active intervention
#' @param adjust (Scalar: Logical) Should the estimand be adjusted for ties?
#' NOTE: This can only be computed when \code{pmf_0} and \code{pmf_1} are
#' supplied.
#'
#' @return When all parameters are scalars, the result is a scalar, indicating
#' the approximate information. When multiple values are specified, a grid of
#' unique parameters are constructed, and the approximate information is
#' computed for each value of the parameters.
#'
#' @export
#'
#' @seealso [asymptotic_information_difference_means] and
#' [asymptotic_information_difference_proportions] for continuous and
#' binary outcomes, respectively.
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
