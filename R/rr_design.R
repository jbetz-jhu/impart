#' Design Calculations for a Relative Risk Estimand
#'
#' This is a function for designing a study with a binary outcome whose
#' inferential target is the relative risk. \code{rr_design()} will calculate
#' the minimum number sample size, relative risk, power, or Type I error when
#' the other arguments are specified. The approximation used relies on the delta
#' method for the log relative risk.
#'
#' The function \code{rr_design} takes in user input, and depending on the
#' arguments supplied, calls the appropriate function depending on whether the
#' sample size (\code{rr_n_per_arm}), power (\code{rr_power}), Type I Error
#' (\code{rr_alpha}), or relative risk (\code{rr_minimal}) is left unspecified.
#'
#' @param n_per_arm A \code{numeric} vector: the sample size in each arm in a
#' 1:1 randomized study
#' @param pi_1 A \code{numeric} vector: the risk of the outcome under treatment
#' @param pi_0 A \code{numeric} vector: the risk of the outcome under control
#' @param rr A \code{numeric} vector: the relative risk of the outcome under
#' treatment relative to control
#' @param rr_above_1 A \code{logical} scalar: search for a relative risk above
#' 1 (\code{TRUE}) or below 1 (\code{FALSE})
#' @param power A \code{numeric} vector: the statistical power of interest
#' @param alpha A \code{numeric} vector: Type I Error probability
#' @param test_sides A \code{numeric} vector: Number of sides for test (1 or 2)
#'
#' @returns a \code{numeric} vector or \code{data.frame}, depending on the
#' arguments supplied and quantity of interest.
#' @export
#'
#' @seealso [stats::power.t.test] for design calculations for a difference in
#' means, [stats::power.prop.test] for design calculations for a difference in
#' proportions, [impart::hr_design] for design calculations for a hazard ratio
#' estimand.
#'
#' @references {
#' Mehta CR, Gao P, Bhatt DL, Harrington RA, Skerjanec S, and Ware JH. 2009.
#' "Optimizing Trial Design: Sequential, Adaptive, and Enrichment Strategies."
#' \emph{Circulation} 119 (4): 597–605.
#' \url{https://doi.org/10.1161/circulationaha.108.809707}.
#' }
#'
#' @examples
#' # Sample Size
#' rr_design(
#'   pi_0 = 0.08,
#'   rr = 0.8,
#'   power = 0.80,
#'   alpha = 0.025,
#'   test_sides = 1
#' )
#'
#' # Power
#' rr_design(
#'   n_per_arm = 4500,
#'   pi_0 = 0.08,
#'   rr = 0.8,
#'   alpha = 0.025,
#'   test_sides = 1
#' )
#'
#' # Type I Error
#' rr_design(
#'   n_per_arm = 4500,
#'   pi_0 = 0.08,
#'   rr = 0.8,
#'   power = 0.80,
#'   test_sides = 1
#' )
#'
#' # Relative Risk: Treatment < Control
#' rr_design(
#'   n_per_arm = 4500,
#'   pi_0 = 0.08,
#'   rr_above_1 = FALSE,
#'   power = 0.80,
#'   alpha = 0.025,
#'   test_sides = 1
#' )
#'
#' # Relative Risk: Treatment > Control
#' rr_design(
#'   n_per_arm = 4500,
#'   pi_0 = 0.08,
#'   rr_above_1 = TRUE,
#'   power = 0.80,
#'   alpha = 0.025,
#'   test_sides = 1
#' )




#' @rdname rr_design
#' @export
rr_n_per_arm <-
  function(
    pi_1 = NULL,
    pi_0 = NULL,
    rr = NULL,
    power = 0.80,
    alpha = 0.05,
    test_sides = 2
  ){
    null_params <-
      is.null(pi_1) + is.null(pi_0) + is.null(rr)

    if(null_params != 1){
      stop("Exactly one of `pi_1`, `pi_0`, and `rr` must be `NULL`.")
    }

    if(is.null(rr)){
      params <-
        expand.grid(
          pi_1 = pi_1,
          pi_0 = pi_0,
          power = power,
          alpha = alpha,
          test_sides = test_sides
        ) |>
        unique()

      params$rr <-
        with(
          data = params,
          expr = {pi_1/pi_0}
        )

    } else if (is.null(pi_1)){
      params <-
        expand.grid(
          rr = rr,
          pi_0 = pi_0,
          power = power,
          alpha = alpha,
          test_sides = test_sides
        ) |>
        unique()

      params$pi_1 <-
        with(
          data = params,
          expr = {pi_0*rr}
        )
    } else if (is.null(pi_0)){
      params <-
        expand.grid(
          rr = rr,
          pi_1 = pi_1,
          power = power,
          alpha = alpha,
          test_sides = test_sides
        ) |>
        unique()

      params$pi_0 <-
        with(
          data = params,
          expr = {pi_1/rr}
        )
    }

    params$relative_risk <- params$rr

    params$z_a <-
      with(
        data = params,
        expr = {qnorm(p = alpha/test_sides)}
      )

    params$z_b <-
      with(
        data = params,
        expr = {qnorm(p = 1 - power)}
      )

    params$n_per_arm <-
      with(
        data = params,
        expr = {
          ((1 - rr*pi_0)/(rr*pi_0) + (1 - pi_0)/(pi_0))*((z_a + z_b)/log(rr))^2
        }
      )

    params$n_total <- 2*params$n_per_arm

    if(nrow(params) > 1){
      return_params <-
        c("pi_1", "pi_0", "relative_risk", "n_per_arm", "n_total",
          "power", "alpha", "test_sides")
      return(params[, return_params])
    } else{
      return(params[, c("n_per_arm", "n_total")])
    }
  }




#' @rdname rr_design
#' @export
# Determine Power
rr_power <-
  function(
    pi_1 = NULL,
    pi_0 = NULL,
    rr = NULL,
    n_per_arm,
    alpha = 0.05,
    test_sides = 2
  ){
    null_params <-
      is.null(pi_1) + is.null(pi_0) + is.null(rr)

    if(null_params != 1){
      stop("Exactly one of `pi_1`, `pi_0`, and `rr` must be `NULL`.")
    }

    if(is.null(rr)){
      params <-
        expand.grid(
          pi_1 = pi_1,
          pi_0 = pi_0,
          n_per_arm = n_per_arm,
          alpha = alpha,
          test_sides = test_sides
        ) |>
        unique()

      params$rr <-
        with(
          data = params,
          expr = {pi_1/pi_0}
        )

    } else if (is.null(pi_1)){
      params <-
        expand.grid(
          rr = rr,
          pi_0 = pi_0,
          n_per_arm = n_per_arm,
          alpha = alpha,
          test_sides = test_sides
        ) |>
        unique()

      params$pi_1 <-
        with(
          data = params,
          expr = {pi_0*rr}
        )
    } else if (is.null(pi_0)){
      params <-
        expand.grid(
          rr = rr,
          pi_1 = pi_1,
          n_per_arm = n_per_arm,
          alpha = alpha,
          test_sides = test_sides
        ) |>
        unique()

      params$pi_0 <-
        with(
          data = params,
          expr = {pi_1/rr}
        )
    }

    params$relative_risk <- params$rr
    params$n_total <- 2*params$n_per_arm

    params$z_a <-
      with(
        data = params,
        expr = {qnorm(p = alpha/test_sides)}
      )

    params$c <-
      with(
        data = params,
        expr = {
          ((1 - rr*pi_0)/(rr*pi_0) + (1 - pi_0)/(pi_0))
        }
      )

    params$power <-
      with(
        data = params,
        expr = {
          1 - pnorm(log(rr)*sqrt(n_per_arm/c) - z_a)
        }
      )

    if(nrow(params) > 1){
      return_params <-
        c("pi_1", "pi_0", "relative_risk", "n_per_arm", "n_total",
          "power", "alpha", "test_sides")
      return(params[, return_params])
    } else{
      return(params[, c("power")])
    }
  }




#' @rdname rr_design
#' @export
# Determine Type I Error
rr_alpha <-
  function(
    pi_1 = NULL,
    pi_0 = NULL,
    rr = NULL,
    n_per_arm,
    power = 0.80,
    test_sides = 2
  ){
    null_params <-
      is.null(pi_1) + is.null(pi_0) + is.null(rr)

    if(null_params != 1){
      stop("Exactly one of `pi_1`, `pi_0`, and `rr` must be `NULL`.")
    }

    if(is.null(rr)){
      params <-
        expand.grid(
          pi_1 = pi_1,
          pi_0 = pi_0,
          n_per_arm = n_per_arm,
          power = power,
          test_sides = test_sides
        ) |>
        unique()

      params$rr <-
        with(
          data = params,
          expr = {pi_1/pi_0}
        )

    } else if (is.null(pi_1)){
      params <-
        expand.grid(
          rr = rr,
          pi_0 = pi_0,
          n_per_arm = n_per_arm,
          power = power,
          test_sides = test_sides
        ) |>
        unique()

      params$pi_1 <-
        with(
          data = params,
          expr = {rr*pi_0}
        )
    } else if (is.null(pi_0)){
      params <-
        expand.grid(
          rr = rr,
          pi_1 = pi_1,
          n_per_arm = n_per_arm,
          power = power,
          test_sides = test_sides
        ) |>
        unique()

      params$pi_0 <-
        with(
          data = params,
          expr = {pi_1/rr}
        )
    }

    params$relative_risk <- params$rr
    params$n_total <- 2*params$n_per_arm

    params$z_b <-
      with(
        data = params,
        expr = {qnorm(p = 1 - power)}
      )

    params$c <-
      with(
        data = params,
        expr = {
          ((1 - rr*pi_0)/(rr*pi_0) + (1 - pi_0)/(pi_0))
        }
      )

    params$alpha <-
      with(
        data = params,
        expr = {
          test_sides*pnorm(log(rr)*sqrt(n_per_arm/c) - z_b)
        }
      )

    if(nrow(params) > 1){
      return_params <-
        c("pi_1", "pi_0", "relative_risk", "n_per_arm", "n_total",
          "power", "alpha", "test_sides")
      return(params[, return_params])
    } else{
      return(params[, c("alpha")])
    }
  }




#' @rdname rr_design
#' @export
# Determine smallest effect on RR scale
rr_minimal <-
  function(
    n_per_arm = NULL,
    pi_0 = NULL,
    rr_above_1,
    power = 0.8,
    alpha = 0.05,
    test_sides = 2
  ){
    if(!inherits(x = rr_above_1, what = "logical")){
      stop("`rr_above_1` must be TRUE or FALSE")
    } else if(!is.finite(rr_above_1) & length(rr_above_1) == 1){
      stop("`rr_above_1` must be TRUE or FALSE")
    }

    if(is.null(pi_0)){
      stop("All elements of `pi_0` must be > 0 and < 1")
    } else if(any(!is.finite(pi_0) | any(pi_0 <= 0) | any(pi_0 >= 1))){
      stop("All elements of `pi_0` must be > 0 and < 1")
    }

    params <-
      expand.grid(
        pi_0 = pi_0,
        rr = NA,
        n_per_arm = n_per_arm,
        power = power,
        alpha = alpha,
        test_sides = test_sides
      ) |>
      unique()

    params$n_total <- 2*params$n_per_arm

    params$z_a <-
      with(
        data = params,
        expr = {qnorm(p = alpha/test_sides)}
      )

    params$z_b <-
      with(
        data = params,
        expr = {qnorm(p = 1 - power)}
      )

    if(rr_above_1){
      lower_rr <- 1 + 1e-6
      upper_rr <- 20
    } else {
      lower_rr <- 1e-6
      upper_rr <- 1 - 1e-6
    }

    for(i in 1:nrow(params)){
      params$rr[i] <-
        with(
          data = params[i,],
          expr = {
            optim(
              par = 0.5,
              fn = function(r, p0, n, za, zb){
                abs(n - (((1 - r*p0)/(r*p0)) + ((1 - p0)/p0))*((za + zb)/log(r))^2)
              },
              n = n_per_arm,
              p0 = pi_0,
              za = z_a,
              zb = z_b,
              method = "Brent",
              lower = lower_rr,
              upper = upper_rr
            )
          }
        )$par
    }

    params$relative_risk <- params$rr

    params$pi_1 <-
      with(
        data = params,
        expr = {rr*pi_0}
      )

    if(nrow(params) > 1){
      return_params <-
        c("pi_1", "pi_0", "relative_risk", "n_per_arm", "n_total",
          "power", "alpha", "test_sides")
      return(params[, return_params])
    } else{
      return(params[, c("relative_risk")])
    }
  }




#' @rdname rr_design
#' @export
# Call appropriate function based on arguments
rr_design <-
  function(
    n_per_arm = NULL,
    pi_1 = NULL,
    pi_0 = NULL,
    rr = NULL,
    rr_above_1 = NULL,
    power = NULL,
    alpha = NULL,
    test_sides = 2
  ){
    if(is.null(test_sides)){
      stop("`test_sides` must be equal to `1` or `2`")
    } else if(!all(test_sides %in% c(1, 2))){
      stop("`test_sides` must be equal to `1` or `2`")
    }

    if(!is.null(n_per_arm)){
      if(any(!is.finite(n_per_arm)) | any(n_per_arm < 1)){
        stop("All elements of `n_per_arm` must be >= 1")
      }
    }

    if(!is.null(rr)){
      if(any(!is.finite(rr)) | any(abs(rr - 1) < 1e-8)){
        stop("All elements of `rr` must be finite and not equal to 1")
      }
    }

    if(!is.null(power)){
      if(any(!is.finite(power)) | any(power <= 0) | any(power >= 1)){
        stop("All elements of `power` must be > 0 and < 1")
      }
    }

    if(!is.null(alpha)){
      if(any(!is.finite(alpha)) | any(alpha <= 0) | any(alpha >= 1)){
        stop("All elements of `alpha` must be > 0 and < 1")
      }
    }

    null_outcome_params <- is.null(pi_0) + is.null(pi_1) + is.null(rr)
    null_design_params <- is.null(power) + is.null(alpha) + is.null(n_per_arm)

    if(null_outcome_params == 0){
      stop("At least one of `pi_1`, `pi_0`, or `rr` must be NULL")
    } else if(null_outcome_params == 1){
      if(null_design_params != 1){
        stop("If risk is specified in each arm, exactly one of `power`, ",
             "`alpha`, or `n_per_arm` must be NULL")
      }
    } else if(null_outcome_params == 2){
      if(is.null(pi_0)){
        stop("To determine RR, `pi_0` must be specified, along with `power`, ",
             "`alpha`, `n_per_arm`, and `rr_above_1`")
      } else if(!inherits(x = rr_above_1, what = "logical")){
        stop("To determine RR, `pi_0` must be specified, along with `power`, ",
             "`alpha`, `n_per_arm`, and `rr_above_1`")
      } else if(null_design_params > 0){
        stop("To determine RR, `pi_0` must be specified, along with `power`, ",
             "`alpha`, and `n_per_arm`")
      }
    } else {
      stop("Unknown error")
    }

    if(is.null(n_per_arm)){
      rr_n_per_arm(
        pi_1 = pi_1,
        pi_0 = pi_0,
        rr = rr,
        power = power,
        alpha = alpha,
        test_sides = test_sides
      )
    } else if(is.null(power)){
      rr_power(
        pi_1 = pi_1,
        pi_0 = pi_0,
        rr = rr,
        n_per_arm = n_per_arm,
        alpha = alpha,
        test_sides = test_sides
      )
    } else if(is.null(alpha)){
      rr_alpha(
        pi_1 = pi_1,
        pi_0 = pi_0,
        rr = rr,
        n_per_arm = n_per_arm,
        power = power,
        test_sides = test_sides
      )
    } else if(is.null(pi_1) & is.null(rr) & !is.null (pi_0)){
      rr_minimal(
        n_per_arm = n_per_arm,
        pi_0 = pi_0,
        rr_above_1 = rr_above_1,
        power = power,
        alpha = alpha,
        test_sides = test_sides
      )
    }
  }
