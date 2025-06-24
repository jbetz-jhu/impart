#' Compute standardization (i.e. G-Computation) estimator
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
#' @param outcome_formula A [stats::formula] specifying the relationship between
#' the outcome and covariates in the combined sample (treatment and control).
#' \strong{Note:} if \code{outcome_formula} is specified, both \code{y0_formula}
#' and \code{y1_formula} must be \code{NULL}.
#' @param y0_formula A [stats::formula] specifying the relationship between the
#' outcome and covariates in the control arm using treatment-stratified outcome
#' models. \strong{Note:} if \code{y0_formula} is specified, \code{y1_formula}
#' must be specified and \code{outcome_formula} must be \code{NULL}.
#' @param y1_formula A [stats::formula] specifying the relationship between the
#' outcome and covariates in the treatment arm using treatment-stratified
#' outcome models. \strong{Note:} if \code{y1_formula} is specified,
#' \code{y0_formula} must be specified and \code{outcome_formula} must be
#' \code{NULL}.
#' @param estimand A \code{character} scalar: "difference" (for a difference in
#' means or risk difference), "ratio" (for a ratio of means or relative risk),
#' or "oddsratio" (for an odds ratio for a binary outcome).
#' @param family The [stats::family] for the outcome regression model

#' @param treatment_column A \code{character} scalar indicating the column
#' containing the treatment indicator.
#' @param outcome_indicator_column A \code{character} scalar indicating the column
#' containing the outcome indicator corresponding to `treatment_column`.
#'
#' @name standardization
#'
#' @return A \code{list} containing the marginal means and their contrast:
#'
#' @export
#'
#' @examples
#'
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

#' @rdname standardization
#' @export
standardization <-
  function(
    data,
    outcome_formula = NULL,
    y0_formula = NULL,
    y1_formula = NULL,
    family,
    estimand = "difference",
    outcome_indicator_column = NULL,
    treatment_column = NULL
  ) {
    if(!(estimand %in% c("difference", "ratio", "oddsratio"))){
      stop("`estimand` must be one of the following: \"difference\", ",
           "\"ratio\", or \"oddsratio\".")
    }

    if(!all(c(treatment_column, outcome_indicator_column) %in% names(data))){
      stop("`treatment_column` (", treatment_column, ") and ",
           "`outcome_indicator_column` (", outcome_indicator_column, ") must ",
           "be in `data`.")
    }

    if(
      !xor(
        x = is.null(outcome_formula),
        y = (is.null(y0_formula) & is.null(y1_formula))
      )
    ) {
      stop(
        "Either `outcome_formula` should be specified or both `y0_formula` and ",
        "`y1_formula` should be specified, not both."
      )
    }

    # Get baseline covariates from `outcome_formula`
    if(!is.null(outcome_formula)){
      baseline_covariates <-
        setdiff(
          x = all.vars(stats::update(old = outcome_formula, new = 0 ~ .)),
          y = treatment_column
        )
    } else {
      baseline_covariates <-
        unique(
          x = c(all.vars(stats::update(old = y0_formula, new = 0 ~ .)),
                all.vars(stats::update(old = y1_formula, new = 0 ~ .)))
        )
    }


    # Impute any missing values using mean/mode imputation
    data <-
      impute_covariates_mean_mode(
        data = data,
        baseline_covariates = baseline_covariates
      )

    # Subset to individuals whose outcomes have been assessed:
    data_assessed <-
      data[which(data[, outcome_indicator_column] %in% c(0, 1)),]

    if(is.null(outcome_formula)){
      formula_list <-
        list(
          y0_formula = y0_formula,
          y1_formula = y1_formula
        )
    } else {
      formula_list <- list(outcome_formula = outcome_formula)
    }

    fn_args <-
      c(
        list(
          data = data,
          estimand = estimand,
          family = family,
          outcome_indicator_column = outcome_indicator_column,
          treatment_column = treatment_column
        ),
        formula_list
      )

    if(!is.null(outcome_formula)){
      do.call(
        what = standardization_tx_formula,
        args = fn_args
      )
    } else if(!(is.null(y0_formula) & is.null(y1_formula))){
      do.call(
        what = standardization_tx_stratified,
        args = fn_args
      )
    }
  }




#' @rdname standardization
#' @export
standardization_tx_formula <-
  function(
    data,
    outcome_formula,
    treatment_column,
    outcome_indicator_column,
    estimand,
    family
  ){
    # Fit a single working model under control and treatment
    outcome_model <-
      stats::glm(
        formula = outcome_formula,
        family = family,
        data = data
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

    out <-
      list(
        estimate = estimate,
        y1_pred = y1_pred,
        y0_pred = y0_pred,
        estimand = estimand
      )

    class(out) <- "estimator"
    return(out)
  }




#' @rdname standardization
#' @export
standardization_tx_stratified <-
  function(
    data,
    y0_formula,
    y1_formula,
    treatment_column,
    outcome_indicator_column,
    estimand,
    family
  ){
    # Fit working models under control and treatment
    y0_mod <-
      stats::glm(
        formula = y0_formula,
        family = family,
        data = data[which(data[, treatment_column] == 0),]
      )

    y1_mod <-
      stats::glm(
        formula = y1_formula,
        family = family,
        data = data[which(data[, treatment_column] == 1),]
      )


    # Make predictions under control for all participants in the dataset
    y0_pred <-
      stats::predict(
        object = y0_mod,
        newdata = data,
        type = "response"
      )

    # Make predictions under treatment for all participants in the dataset
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
      estimate <-
        (mean(y1_pred)/(1 - mean(y1_pred))) /
        (mean(y0_pred)/(1 - mean(y0_pred)))
    }

    out <-
      list(
        estimate = estimate,
        y1_pred = y1_pred,
        y0_pred = y0_pred,
        estimand = estimand
      )

    class(out) <- "estimator"
    return(out)
  }




#' @rdname standardization
#' @export
standardization_correction <-
  function(
    data,
    outcome_formula = NULL,
    y0_formula = NULL,
    y1_formula = NULL,
    treatment_column,
    outcome_indicator_column
  ){

    if(
      !xor(
        x = is.null(outcome_formula),
        y = (is.null(y0_formula) & is.null(y1_formula))
      )
    ) {
      stop(
        "Either `outcome_formula` should be specified or both `y0_formula` ",
        "and `y1_formula` should be specified, not both."
      )
    }

    stratified <- is.null(outcome_formula)

    control_rows <- which(data[, treatment_column] == 0)
    treatment_rows <- which(data[, treatment_column] == 1)

    n_0 <- sum(data[control_rows, outcome_indicator_column], na.rm = TRUE)
    n_1 <- sum(data[treatment_rows, outcome_indicator_column], na.rm = TRUE)

    if(stratified){
      p_0 <-
        stats::model.matrix(
          object = y0_formula,
          data = data
        ) |>
        ncol()

      p_1 <-
        stats::model.matrix(
          object = y1_formula,
          data = data
        ) |>
        ncol()

      return((1/(n_1 - p_1) + 1/(n_0 - p_0))/(1/(n_1 - 1) + 1/(n_0 - 1)))
    } else {
      p <-
        stats::model.matrix(
          object = outcome_formula,
          data = data
        ) |>
        ncol()

      return((n_0 + n_1 - 1)/(n_0 + n_1 - p))
    }
  }
