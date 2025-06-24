# Design Calculations: Hazard Ratio as Estimand
#'
#' Calculate minimum hazard ratio, number of required events, power, or Type I error
#'
#' This is a function for designing a study with a time-to-event outcome whose
#' inferential target is the hazard ratio. \code{hr_design()} will calculate
#' the minimum number of events, minimum hazard ratio, power, or Type I error
#' when the other arguments are specified.
#'
#' @param events A \code{numeric} vector: the number of observed events
#' @param hazard_ratio A \code{numeric} vector: the minimum hazard ratio of interest
#' @param power A \code{numeric} vector: the statistical power of interest
#' @param alpha A \code{numeric} vector: Type I Error probability
#' @param test_sides A \code{numeric} vector: Number of sides for test (1 or 2)
#' @param ratio A \code{numeric} vector: the allocation ratio of treatment to
#' control, i.e. for an \eqn{r:1} trial, \code{ratio} = \eqn{r}.
#'
#' @returns a \code{numeric} vector or \code{data.frame}, depending on whether
#' arguments are specified as scalars or vectors.
#'
#' @examples
#' # Number of Events
#' hr_design(
#'   hazard_ratio = 0.75,
#'   power = 0.80,
#'   alpha = 0.05,
#'   test_sides = 2
#' )
#'
#' # Power
#' hr_design(
#'   events = 400,
#'   hazard_ratio = 0.75,
#'   alpha = 0.05,
#'   test_sides = 2
#' )
#'
#' # Type I Error
#' hr_design(
#'   events = 400,
#'   hazard_ratio = 0.75,
#'   power = 0.80,
#'   test_sides = 2
#' )
#'
#' # Hazard Ratio
#' hr_design(
#'   events = 400,
#'   power = 0.80,
#'   alpha = 0.05,
#'   test_sides = 2
#' )

#' @rdname hr_design
#' @export
hr_events <-
  function(
    hazard_ratio,
    power = 0.8,
    alpha = 0.05,
    test_sides = 2,
    ratio = 1
  ){
    params <-
      expand.grid(
        hazard_ratio = hazard_ratio,
        power = power,
        alpha = alpha,
        test_sides = test_sides,
        ratio = ratio
      ) |>
      unique()

    params$hr_lt_1 <-
      with(
        data = params,
        expr = {
          ifelse(
            test = hazard_ratio > 1,
            yes = 1/hazard_ratio,
            no = hazard_ratio
          )
        }
      )

    params$z_a <-
      with(
        data = params,
        expr = {qnorm(p = alpha/test_sides)}
      )

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

    params$events <-
      with(
        data = params,
        expr = {
          (1/ratio)*(1 + ratio)^2*((z_a + z_b)/log(hr_lt_1))^2
        }
      )

    if(nrow(params) > 1){
      return_params <-
        c("events", "hazard_ratio", "power", "alpha", "test_sides", "ratio")
      return(params[, return_params])
    } else{
      return(params$events)
    }
  }

#' @rdname hr_design
#' @export
# Determine Power
hr_power <-
  function(
    events,
    hazard_ratio,
    alpha = 0.05,
    test_sides = 2,
    ratio = 1
  ){
    params <-
      expand.grid(
        events = events,
        hazard_ratio = hazard_ratio,
        alpha = alpha,
        test_sides = test_sides,
        ratio = ratio
      ) |>
      unique()

    params$hr_lt_1 <-
      with(
        data = params,
        expr = {
          ifelse(
            test = hazard_ratio > 1,
            yes = 1/hazard_ratio,
            no = hazard_ratio
          )
        }
      )

    params$z_a <-
      with(
        data = params,
        expr = {qnorm(p = alpha/test_sides)}
      )

    params$power <-
      with(
        data = params,
        expr = {
          1 - pnorm(log(hr_lt_1)*sqrt((ratio/(1 + ratio)^2)*events) - z_a)
        }
      )

    if(nrow(params) > 1){
      return_params <-
        c("events", "hazard_ratio", "power", "alpha", "test_sides", "ratio")
      return(params[, return_params])
    } else{
      return(params$power)
    }
  }

#' @rdname hr_design
#' @export
# Determine Type I Error
hr_alpha <-
  function(
    events,
    hazard_ratio,
    power = 0.80,
    test_sides = 2,
    ratio = 1
  ){
    params <-
      expand.grid(
        events = events,
        hazard_ratio = hazard_ratio,
        power = power,
        test_sides = test_sides,
        ratio = ratio
      ) |>
      unique()

    params$hr_lt_1 <-
      with(
        data = params,
        expr = {
          ifelse(
            test = hazard_ratio > 1,
            yes = 1/hazard_ratio,
            no = hazard_ratio
          )
        }
      )

    params$z_b <-
      with(
        data = params,
        expr = {qnorm(p = 1 - power)}
      )

    params$alpha <-
      with(
        data = params,
        expr = {
          test_sides*pnorm(
            log(hr_lt_1)*sqrt((ratio/(1 + ratio)^2)*events) - z_b
          )
        }
      )

    if(nrow(params) > 1){
      return_params <-
        c("events", "hazard_ratio", "power", "alpha", "test_sides", "ratio")
      return(params[, return_params])
    } else{
      return(params$alpha)
    }
  }

#' @rdname hr_design
#' @export
# Determine minimum absolute value of HR detectable
hr_minimal <-
  function(
    events,
    power = 0.80,
    alpha = 0.05,
    test_sides = 2,
    ratio = 1
  ){
    params <-
      expand.grid(
        events = events,
        power = power,
        alpha = alpha,
        test_sides = test_sides,
        ratio = ratio
      ) |>
      unique()

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

    params$hazard_ratio <-
      with(
        data = params,
        expr = {
          exp((z_a + z_b)/sqrt((ratio/(1 + ratio)^2)*events))
        }
      )

    params$hazard_ratio_inv <- 1/params$hazard_ratio


    if(nrow(params) > 1){
      return_params <-
        c("events", "hazard_ratio", "hazard_ratio_inv", "power", "alpha", "test_sides", "ratio")
      return(params[, return_params])
    } else{
      return(params[, c("hazard_ratio", "hazard_ratio_inv")])
    }
  }

#' @rdname hr_design
#' @export
# Call appropriate function based on arguments
hr_design <-
  function(
    events = NULL,
    hazard_ratio = NULL,
    power = NULL,
    alpha = NULL,
    test_sides = 2,
    ratio = 1
  ){
    if(is.null(test_sides)){
      stop("`test_sides` must be equal to `1` or `2`")
    } else if(!all(test_sides %in% c(1, 2))){
      stop("`test_sides` must be equal to `1` or `2`")
    }

    if(is.null(ratio)){
      stop("`ratio` must be numeric and positive")
    } else if(any(!is.finite(ratio)) | any(ratio <= 0)){
      stop("`ratio` must be numeric and positive")
    }

    if(!is.null(events)){
      if(any(!is.finite(events)) | any(events < 1)){
        stop("All elements of `events` must be >= 1")
      }
    }

    if(!is.null(hazard_ratio)){
      if(any(!is.finite(hazard_ratio)) | any(abs(hazard_ratio - 1) < 1e-8)){
        stop("All elements of `hazard_ratio` must be finite and not equal to 1")
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

    null_params <-
      is.null(events) + is.null(hazard_ratio) + is.null(power) +
      is.null(alpha)

    if(null_params != 1){
      stop("Exactly one of `events`, `hazard_ratio`, `power`, and `alpha` ",
           "must be `NULL`.")
    } else if(is.null(events)){
      hr_events(
        hazard_ratio = unique(hazard_ratio),
        power = unique(power),
        alpha = unique(alpha),
        test_sides = unique(test_sides),
        ratio = unique(ratio)
      )
    } else if(is.null(hazard_ratio)){
      hr_minimal(
        events = unique(events),
        power = unique(power),
        alpha = unique(alpha),
        test_sides = unique(test_sides),
        ratio = unique(ratio)
      )
    } else if(is.null(power)){
      hr_power(
        events = unique(events),
        hazard_ratio = unique(hazard_ratio),
        alpha = unique(alpha),
        test_sides = unique(test_sides),
        ratio = unique(ratio)
      )
    } else if(is.null(alpha)){
      hr_alpha(
        events = unique(events),
        hazard_ratio = unique(hazard_ratio),
        power = unique(power),
        test_sides = unique(test_sides),
        ratio = unique(ratio)
      )
    }
  }
