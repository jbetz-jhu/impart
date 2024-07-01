#' Compute stanadardization (i.e. G-Computation) estimator
#'
#' Computes the estimate of a contrast of means for continuous and binary
#' outcomes on the additive scale, relative scale. Computing a marginal odds
#' ratio is also possible for binary outcomes.
#'
#' Generalized linear models, stratified by treatment arm, are fitted using the
#' specified formulas. Predictions are generated for each individual from each
#' treatment model, representing the predicted outcome under each treatment
#' assignment. These predictions are marginalized over the covariates by
#' averaging to produce marginal estimates of the means. Finally, the contrast
#' is computed and returned.
#'
#' Variance estimates can be obtained using the nonparametric bootstrap.
#' Unadjusted estimates can be obtained by using intercept only models.
#'
#' @param data A \code{data.frame} containing baseline covariates (e.g. `x1`,
#' `x2`, ...), a binary treatment indicator (e.g. `tx` where 1 = Treatment;
#' 0 = Control), outcome variables (e.g. `y1`, `y2`, ...), and outcome
#' indicators (e.g. `.r_1`, `.r_2`, ...). The outcome indicators indicate
#' whether an outcome has been observed (`1`), is missing (`0`), or not yet
#' obtained (\code{NA}).
#' @param estimand A \code{character} scalar: "difference" (for a difference in
#' means or risk difference), "ratio" (for a ratio of means or relative risk),
#' or "oddsratio" (for an odds ratio for a binary outcome).
#' @param y0_formula A [stats::formula] specifying the relationship between the
#' outcome and covariates in the control arm.
#' @param y1_formula A [stats::formula] specifying the relationship between the
#' outcome and covariates in the treatment arm.
#' @param family The [stats::family] for the outcome regression model
#' @param treatment_column A \code{character} scalar indicating the column
#' containing the treatment indicator.
#' @param outcome_indicator_column A \code{character} scalar indicating the column
#' containing the outcome indicator corresponding to `treatment_column`.
#'
#' @return A \code{list} containing the marginal means and their contrast:
#' @export
#'
#' @examples
#' ex_1 <- example_1
#' ex_1$.r_4 <- 1*(!is.na(ex_1$y_4))
#'
#' standardization(
#'   data = ex_1,
#'   estimand = "difference",
#'   y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
#'   y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
#'   family = gaussian,
#'   treatment_column = "tx",
#'   outcome_indicator_column = ".r_4"
#' )
#'

standardization <-
  function(
    data,
    estimand = "difference",
    y0_formula,
    y1_formula,
    family,
    treatment_column = NULL,
    outcome_indicator_column = NULL
  ){

    if(!(estimand %in% c("difference", "ratio", "oddsratio"))){
      stop("`estimand` must be one of the following: \"difference\", \"ratio\", ",
           "or \"oddsratio\".")
    }

    if(!all(c(treatment_column, outcome_indicator_column) %in% names(data))){
      stop("`treatment_column` (", treatment_column, ") and ",
           "`outcome_indicator_column` (", outcome_indicator_column, ") must ",
           "be in `data`.")
    }

    # Subset to individuals whose outcomes have been assessed:
    data_assessed <-
      data[which(data[, outcome_indicator_column] %in% c(0, 1)),]

    # Fit working models under control and treatment
    y0_mod <-
      stats::glm(
        formula = y0_formula,
        family = family,
        data = data_assessed[which(data_assessed[, treatment_column] == 0),]
      )

    y1_mod <-
      stats::glm(
        formula = y1_formula,
        family = family,
        data = data_assessed[which(data_assessed[, treatment_column] == 1),]
      )


    # Make predictions under control and treatment for all patients in the dataset
    y0_pred <-
      stats::predict(
        object = y0_mod,
        newdata = data,
        type = "response"
      )

    y1_pred <-
      stats::predict(
        object = y1_mod,
        newdata = data,
        type = "response"
      )

    # Estimate treatment effect
    if(estimand == "difference"){
      estimate = mean(y1_pred) - mean(y0_pred)
    } else if(estimand == "ratio"){
      estimate = mean(y1_pred)/mean(y0_pred)

    } else if(estimand == "oddsratio"){
      estimate = (mean(y1_pred)/(1-mean(y1_pred)))/
        (mean(y0_pred)/(1-mean(y0_pred)))
    }

    out <-
      list(
        estimate = estimate,
        y1_pred = y1_pred,
        y0_pred = y0_pred
      )

    class(out) <- "standardization"
    return(out)
  }
