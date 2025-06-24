#' Wrapper for speff2trial::speffsurv: See [speff2trial::speffSurv]
#'
#' @param data A \code{data.frame}
#' @param estimand A \code{character} vector - must be "log_hazard_ratio" (default)
#' @param formula A \code{formula} for covariate adjustment.
#' @param treatment_column A \code{character} scalar, indicating the column containing the treatment indicator.
#' @param alpha A \code{numeric} scalar, indicating Type I error rate
#' @param ci A \code{logical} scalar: should CI be returned
#'
#' @returns A \code{list} containing the estimate, standard error, estimand,
#' and confidence interval (if requested).
#'
#' @export
#'
#' @examples
#' speffsurv_impart(
#'   data = sim_colon_cancer,
#'   estimand = "log_hazard_ratio",
#'   formula =
#'     survival::Surv(time = years_to_death, event = event_death) ~
#'     age + sex + obstruction + perforation + organ_adherence + positive_nodes +
#'     differentiation + local_spread,
#'   treatment_column = "tx",
#'   alpha = 0.05,
#'   ci = FALSE
#' )

speffsurv_impart <-
  function(
    data,
    estimand = "log_hazard_ratio",
    formula,
    treatment_column = NULL,
    alpha = 0.05,
    ci = FALSE
  ){
    if(!(estimand %in% c("log_hazard_ratio"))){
      stop("`estimand` must be \"log_hazard_ratio\".")
    }

    if(!(treatment_column %in% names(data))){
      stop("`treatment_column` (", treatment_column, ") must be in `data`.")
    }

    # Check for event indicators that are missing times, vice versa
    outcome_cols <-
      all.vars(stats::update(old = formula, new = . ~ 0))

    if(length(outcome_cols == 2)){
      miss_rows <- which(rowSums(is.na(data[, outcome_cols])) == 1)
      if(length(miss_rows) > 0){
        stop("Indicators missing an event time or event times missing an ",
             "outcome indicator: rows ", paste0(miss_rows, collapse = ", "))
      }
    }

    # Get baseline covariates from formula
    baseline_covariates <-
      all.vars(stats::update(old = formula, new = 0 ~ .))

    # Impute any missing values using mean/mode imputation
    data <-
      impute_covariates_mean_mode(
        data = data,
        baseline_covariates = baseline_covariates
      )

    # Subset to individuals whose outcomes have been assessed:
    speffsurv_result <-
      speff2trial::speffSurv(
        formula = formula,
        data = data,
        trt.id = treatment_column,
        conf.level = 1 - alpha,
        fixed = TRUE
      )

    # Return CI if specified
    if(ci){
      speffsurv_summary <-
        data.frame(summary(speffsurv_result)$tab)
      lcl <- speffsurv_summary["Speff", "LowerCI"]
      ucl <- speffsurv_summary["Speff", "UpperCI"]
    } else {
      lcl = NULL
      ucl = NULL
    }

    return(
      list(
        estimate = as.numeric(speffsurv_result$beta["Speff"]),
        se = sqrt(as.numeric(speffsurv_result$varbeta["Speff"])),
        var = as.numeric(speffsurv_result$varbeta["Speff"]),
        lcl = lcl,
        ucl = ucl,
        alpha = alpha,
        estimand = estimand
      )
    )
  }
