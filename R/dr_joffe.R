#' Doubly Robust Estimation Using Joffe's Doubly Robust Weighted Least Squares (DR-WLS) Estimator
#'
#' Doubly robust estimators provide a consistent estimate of their target if
#' either the outcome model or censoring model are correctly specified.
#'
#' @param data A \code{data.frame}
#' @param outcome_formula A \code{formula} for the outcome model - passed to
#' \code{\link[stats]{glm}}
#' @param outcome_family A \code{family} for the outcome model - passed to
#' \code{\link[stats]{glm}}
#' @param treatment_formula A \code{formula}: the left-hand side should be a
#' binary variable indicating assignment to treatment, and the right-hand side
#' should be an intercept (e.g. \code{tx ~ 1})
#' @param missing_formula  A \code{formula} for the censoring model - passed to
#' \code{\link[stats]{glm}}
#' @param missing_family A \code{family} for the censoring model - passed to
#' \code{\link[stats]{glm}}
#' @param estimand A \code{character} scalar specifying the estimate to compute,
#' such as \code{"difference"}, \code{"ratio"}, \code{"oddsratio"}
#' @param se_method A \code{character} scalar indicating how standard errors
#' and confidence intervals should be calculated: "none" only computes the
#' estimate, the "sandwich" method use asymptotic variance estimates.
#' @param alpha A \code{numeric} scalar, indicating the Type I error rate
#' (1 - Confidence Level)
#' @param bootstrap_parameters A \code{list} of arguments to be passed to
#' \code{\link[boot]{boot}} for computing the bootstrap.
#' @param bootstrap_ci_method A \code{character} vector, indicating the
#' method(s) used for computing confidence intervals. See
#' \code{\link[boot]{boot.ci}} for details.
#' @param verbose A \code{logical} scalar: should just the estimate be returned
#' (\code{FALSE}), or should a \code{list} be returned, containing intermediate
#' results (\code{TRUE})? Defaults to \code{FALSE}.
#'
#' @returns A \code{numeric} scalar, a \code{data.frame}, or a \code{list},
#' depending on the options \code{se_method} and \code{verbose}.
#'
#' @examples
#' # To be added


#' @rdname dr_joffe
#' @export
dr_joffe <-
  function(
    data,
    outcome_formula,
    outcome_family,
    treatment_formula,
    missing_formula,
    missing_family = quasibinomial,
    estimand = c("difference", "ratio", "oddsratio")[1],
    se_method = c("none", "bootstrap")[1],
    alpha = 0.05,
    bootstrap_parameters = bootstrap_parameters(),
    bootstrap_ci_method = "bca",
    verbose = FALSE
  ){
    if(!inherits(x = outcome_formula, what = "formula")){
      stop("`outcome_formula` must be a formula")
    }

    if(!inherits(x = missing_formula, what = "formula")){
      stop("`missing_formula` must be a formula")
    }

    if(!inherits(x = treatment_formula, what = "formula")){
      stop("`treatment_formula` must be a formula")
    }

    # Extract variables in formulas: check for missing covariates
    treatment_column <-
      all.vars(expr = update.formula(old = treatment_formula, new = . ~ 1))

    outcome_column <-
      all.vars(expr = update.formula(old = outcome_formula, new = . ~ 1))

    outcome_covariates <-
      formula(x = delete.response(termobj = terms(x = outcome_formula))) |>
      all.vars()

    missing_column <-
      all.vars(expr = update.formula(old = missing_formula, new = . ~ 1))

    missing_covariates <-
      formula(x = delete.response(termobj = terms(x = missing_formula))) |>
      all.vars()

    predictors <-
      setdiff(
        x = unique(c(outcome_covariates, missing_covariates)),
        y = treatment_column
      )

    all_vars <-
      c(outcome_covariates, missing_covariates, treatment_column,
        outcome_column, missing_column) |>
      unique()

    if(!all(all_vars %in% names(data))){
      stop(
        "`formula` contains variables not included in `data`: ",
        setdiff(x = all.vars(expr = formula), y = names(data))
      )
    }

    if(!treatment_column %in% outcome_covariates){
      stop(
        "`treatment_column` (", treatment_column, ") must be in ",
        "`outcome_formula` (",
        Reduce(f = paste, x = deparse(expr = outcome_formula)), ")"
      )
    }

    if(any(is.na(data[, treatment_column]))){
      stop(
        "Treatment assignment (", treatment_column,") should not be missing."
      )
    }

    if(any(is.na(data[, outcome_column]) & data[, missing_column] %in% c(1))){
      stop("Missing outcome indicated as observed (1)")
    }

    if(any(!is.na(data[, outcome_column]) &
           data[, missing_column] %in% c(0, NA))){
      stop("Non-missing outcome indicated as missing (1) or not-yet-observed (NA)")
    }

    if(!(estimand %in% c("difference", "ratio", "oddsratio"))){
      stop("`estimand` must be one of the following: \"difference\", ",
           "\"ratio\", or \"oddsratio\".")
    }

    if(!(se_method %in% c("none", "bootstrap"))){
      stop("`se_method` must be one of the following: \"none\" or \"bootstrap\".")
    }

    if(se_method == "none"){
      return(
        dr_joffe_fit(
          data = data,
          outcome_formula = outcome_formula,
          outcome_family = outcome_family,
          treatment_column = treatment_column,
          missing_formula = missing_formula,
          missing_family = missing_family,
          missing_column = missing_column,
          predictors = predictors,
          estimand = estimand,
          verbose = verbose
        )
      )
    } else if(se_method %in% "bootstrap"){
      boot_result <-
        do.call(
          what = boot::boot,
          args =
            c(
              list(
                data = data,
                statistic =
                  function(data, indices, ...) {
                    dr_joffe_fit(
                      data = data[indices,],
                      ...
                    )
                  },
                outcome_formula = outcome_formula,
                outcome_family = outcome_family,
                treatment_column = treatment_column,
                missing_formula = missing_formula,
                missing_family = missing_family,
                missing_column = missing_column,
                predictors = predictors,
                estimand = estimand,
                verbose = FALSE
              ),
              bootstrap_parameters
            )
        )

      gc_fit <-
        dr_joffe_fit(
          data = data,
          outcome_formula = outcome_formula,
          outcome_family = outcome_family,
          treatment_column = treatment_column,
          missing_formula = missing_formula,
          missing_family = missing_family,
          missing_column = missing_column,
          predictors = predictors,
          estimand = estimand,
          verbose = TRUE
        )

      variance_factor <- gc_fit$variance_factor

      boot_result$t <-
        with(
          data = boot_result,
          mean(t) + (t - mean(t))*sqrt(variance_factor)
        )

      boot_result_ci <-
        boot::boot.ci(
          boot.out = boot_result,
          conf = 1 - alpha,
          type = bootstrap_ci_method,
          outcome_formula = outcome_formula,
          outcome_family = outcome_family,
          treatment_column = treatment_column,
          missing_formula = missing_formula,
          missing_family = missing_family,
          estimand = estimand,
          verbose = FALSE
        )

      ci_methods <-
        setdiff(
          x = names(boot_result_ci),
          y = c("R", "t0", "call")
        )

      ci_results <- list()
      for(i in 1:length(ci_methods)){
        ci_i_unadj <-
          utils::tail(
            x = get(x = ci_methods[i], pos = boot_result_ci)[1,],
            n = 2
          )

        ci_i_adj <-
          boot_result$t0 +
          sqrt(variance_factor)*(ci_i_unadj - boot_result$t0)

        ci_results[[i]] <-
          stats::setNames(
            object =
              c(ci_i_unadj, ci_i_adj),
            nm =
              c(
                paste0(c("lcl", "ucl"), "_", ci_methods[i], "_", 1 - alpha),
                paste0(c("lcl", "ucl"), "_", ci_methods[i],
                       "_df_adjusted_", 1 - alpha)
              )
          )
      }

      boot_result_ci <-
        data.frame(
          estimate = as.numeric(boot_result$t0),
          se_boot = sd(boot_result$t),
          se_boot_df_adjusted = sd(boot_result$t*sqrt(variance_factor)),
          t(do.call(what = c, args = ci_results)),
          estimand = estimand
        )

      if(verbose){
        return(
          list(
            result = boot_result_ci,
            joffe_fit = gc_fit,
            estimand = estimand,
            boot_object = boot_result
          )
        )
      } else {
        return(boot_result_ci)
      }
    }
  }


#' @param treatment_column A \code{character} scalar containing the name of
#' the treatment assignment variable
#' @param missing_column A \code{character} scalar containing the name of
#' the outcome missingness indicator variable
#' @param predictors A \code{character} vector containing the name of
#' covariates in either the outcome or censoring models.
#' @rdname dr_joffe
#' @export

dr_joffe_fit <-
  function(
    data,
    outcome_formula,
    outcome_family,
    treatment_column,
    missing_formula,
    missing_family = quasibinomial,
    missing_column,
    predictors,
    estimand = c("difference", "ratio", "oddsratio")[1],
    verbose = FALSE
  ){
    if(any(is.na(data[, predictors]))){
      # Impute any missing values using mean/mode imputation
      data <-
        impute_covariates_mean_mode(
          data = data,
          baseline_covariates = predictors
        )
    }

    missing_model <-
      stats::glm(
        formula = missing_formula,
        family = missing_family,
        data = data[which(data[, missing_column] %in% c(0, 1)),]
      )

    pr_missing <-
      predict(
        object = missing_model,
        newdata = data,
        type = "response"
      )

    data$.weight <- 1/pr_missing

    outcome_model <-
      with(
        data = data,
        expr = {
          stats::glm(
            formula = outcome_formula,
            family = outcome_family,
            data = data,
            weight = .weight
          )
        }
      )

    y0_pred <-
      stats::predict(
        object = outcome_model,
        newdata =
          within(
            data = data,
            assign(
              x = treatment_column,
              value = 0
            )
          ),
        type = "response"
      )

    y1_pred <-
      stats::predict(
        object = outcome_model,
        newdata =
          within(
            data = data,
            assign(
              x = treatment_column,
              value = 1
            )
          ),
        type = "response"
      )

    # Estimate treatment effect
    if(estimand == "difference"){
      estimate = mean(y1_pred) - mean(y0_pred)
    } else if(estimand == "ratio"){
      estimate = mean(y1_pred)/mean(y0_pred)

    } else if(estimand == "oddsratio"){
      estimate <-
        (mean(y1_pred)/(1 - mean(y1_pred))) /
        (mean(y0_pred)/(1 - mean(y0_pred)))
    }

    return(
      if(verbose){
        n_observations <- length(outcome_model$residuals)

        n_covariates <-
          length(
            setdiff(
              x = attributes(terms(outcome_formula))$term.labels,
              y = treatment_column
            )
          )

        outcome <-
          all.vars(update.formula(old = outcome_formula, new = . ~ 0))

        y_pred <-
          stats::predict(
            object = outcome_model,
            newdata = data,
            type = "response"
          )

        list(
          estimate = estimate,
          y = data[, outcome],
          a = data[, treatment_column],
          y_pred = y_pred,
          y1_pred = y1_pred,
          y0_pred = y0_pred,
          estimand = estimand,
          n_observations = n_observations,
          n_covariates = n_covariates,
          variance_factor =
            (n_observations - 1)/(n_observations - n_covariates - 1)
        )
      } else {
        estimate
      }
    )
  }

