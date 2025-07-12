#' Estimate Treatment Effect using Marginal Standardization (G-Computation)
#'
#' Compute an estimate of a marginal treatment effect using marginal
#' standardization, also known as standardization or G-computation.
#'
#' Marginal standardization can be viewed as a generalization of the ANCOVA
#' estimate: the estimated treatment effect will coincide with ANCOVA when a
#' single outcome model with an identity link is fit using a functional form
#' where treatment assignment only appears as a main effect (i.e. no
#' treatment-by-covariate interactions).
#'
#' Outcome regressions can include a single generalized linear model, or two
#' treatment-stratified generalized linear models. Variance estimates can be
#' obtained through the nonparametric bootstrap. Variance estimation using the
#' influence or score function can also be obtained for certain estimands and
#' outcome models.
#'
#' \code{standardization} should be used: this checks inputs, and calls
#' \code{standardization_fit}: the latter has been streamlined to speed up
#' the nonparametric bootstrap.
#'
#' @param data A \code{data.frame} containing the data to be analyzed.
#' @param outcome_formula A \code{formula} which includes treatment as a main
#' effect. To use treatment-stratified outcome models, leave this argument
#' \code{NULL}.
#' @param outcome_formula_treatment A \code{formula} for a regression model fit
#' to participants in the treatment arm. If using a single model with regression
#' parameters for treatment assignment, leave this argument \code{NULL}.
#' @param outcome_formula_control A \code{formula} for a regression model fit
#' to participants in the control arm. If using a single model with regression
#' parameters for treatment assignment, leave this argument \code{NULL}.
#' @param family The \code{family} for the regression model(s) being fit.
#' See \code{?family}.
#' @param estimand A \code{character} scalar, indicating the estimand to be
#' returned: \code{"difference"} for a difference in means (or risk difference);
#' \code{"ratio"} for a ratio of means (or relative risk), or \code{"oddsratio"}
#' for an odds ratio.
#' @param treatment_column A \code{character} scalar indicating the column that
#' contains the binary indicator of treatment assignment.
#' @param se_method A \code{character} scalar, indicating the method for
#' computing standard errors: \code{"none"} for no standard errors;
#' @param alpha Confidence level for computing interval estimates. This is only
#' used when \code{se_method != "none"}.
#' @param bootstrap_parameters A \code{list} of parameters to be passed to
#' \code{boot::boot()} when computing bootstrap standard errors and confidence
#' intervals. See \code{?boot::boot()}. This is only used when
#' \code{se_method == "bootstrap"}.
#' @param bootstrap_ci_method A \code{character} scalar, specifying the type of
#' interval estimates to compute. See \code{?boot::boot()}. This is only used
#' when \code{se_method == "bootstrap"}.
#' @param variance_adjustment A \code{function} for computing a finite-sample
#' variance adjustment for degrees of freedom in model fitting.
#' @param influence_function A \code{function} for computing the influence
#' function of each observation, which can be used for variance and covariance
#' calculations.
#' @param verbose A \code{logical} scalar: if \code{FALSE}, only results are
#' returned; if \code{TRUE}, additional objects used to construct the estimate
#' are also returned.
#'
#' @name standardization
#'
#' @returns Depending on the options specified, a \code{numeric} scalar
#' containing the estimate, a \code{data.frame} containing estimates, standard
#' errors, and confidence intervals, or a \code{list} additionally containing
#' objects used in constructing the estimates.
#' @export
#'
#' @examples
#' impart::standardization(
#'   data = example_1,
#'   outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
#'   family = gaussian(link = "identity"),
#'   estimand = "difference",
#'   treatment_column = "tx",
#'   se_method = "influence"
#' )
#'
#' impart::standardization(
#'   data = example_1,
#'   outcome_formula = y_4 ~ x_1 + x_2 + x_3 + x_4 + tx,
#'   family = gaussian(link = "identity"),
#'   estimand = "difference",
#'   treatment_column = "tx",
#'   se_method = "score"
#' )

standardization <-
  function(
    data,
    outcome_formula = NULL,
    outcome_formula_treatment = NULL,
    outcome_formula_control = NULL,
    family,
    estimand = "difference",
    treatment_column = NULL,
    se_method = c("none", "influence", "score", "bootstrap")[1],
    alpha = 0.05,
    bootstrap_parameters = impart::boot_control(),
    bootstrap_ci_method = "bca",
    variance_adjustment = standardization_df_adjust_tsiatis_2008,
    influence_function = standardization_influence,
    verbose = FALSE
  ){

    if(!(estimand %in% c("difference", "ratio", "oddsratio"))){
      stop("`estimand` must be one of the following: \"difference\", ",
           "\"ratio\", or \"oddsratio\".")
    }

    if(!(se_method %in% c("none", "influence", "score", "bootstrap"))){
      stop("`se_method` must be one of the following: \"none\", ",
           "\"influence\", \"score\", or \"bootstrap\".")
    }

    if(
      !xor(
        x = inherits(x = outcome_formula, what = "formula"),
        y = all(inherits(x = outcome_formula_treatment, what = "formula"),
                inherits(x = outcome_formula_control, what = "formula"))
      )
    ){
      stop("Either `outcome_formula` must be a formula, or both ",
           "`outcome_formula_treatment` and `outcome_formula_control` must ",
           "be formulas.")
    }

    # Get Outcome
    if(is.null(outcome_formula)){
      outcome_column_1 <-
        all.vars(
          expr = update.formula(old = outcome_formula_treatment, new = . ~ 1)
        )

      outcome_column_0 <-
        all.vars(
          expr = update.formula(old = outcome_formula_control, new = . ~ 1)
        )

      if(!identical(x = outcome_column_1, y = outcome_column_0)){
        stop("Left hand side of `outcome_formula_treatment` and ",
             "`outcome_formula_control` must be identical.")
      }

      outcome_column <- outcome_column_1
    } else {
      outcome_column <-
        all.vars(expr = update.formula(old = outcome_formula, new = . ~ 1))
    }

    # Get Predictors
    if(is.null(outcome_formula)){
      predictors_treatment <-
        formula(
          x = delete.response(termobj = terms(x = outcome_formula_treatment))
        ) |>
        all.vars()

      predictors_control <-
        formula(
          x = delete.response(termobj = terms(x = outcome_formula_control))
        ) |>
        all.vars()

      predictors <- unique(x = c(predictors_treatment, predictors_control))
    } else {
      predictor_terms <-
        attributes(terms(x = outcome_formula))$factors

      predictor_term_rows <-
        (rowSums(predictor_terms) > 0) & # Not an outcome
        (rownames(predictor_terms) != treatment_column) # Not Tx/Interaction

      predictors <- rownames(predictor_terms)[predictor_term_rows]
    }

    all_vars <-
      c(predictors, treatment_column, outcome_column) |>
      unique()

    if(!all(all_vars %in% names(data))){
      stop(
        "Formula contains variables not included in `data`: ",
        setdiff(x = all_vars, y = names(data))
      )
    }

    standardization_result <-
      do.call(
        what = standardization_fit,
        args =
          list(
            data,
            outcome_formula_treatment = outcome_formula_treatment,
            outcome_formula_control = outcome_formula_control,
            outcome_formula = outcome_formula,
            family = family,
            predictors = predictors,
            estimand = estimand,
            treatment_column = treatment_column,
            verbose = verbose | se_method != "none"
          )
      )

    # Compute Standard Error (if applicable)
    if(se_method == "none"){
      return(c("estimate" = standardization_result))
    } else {
      # Calculate degree-of-freedom adjustment for variance
      if(is.null(variance_adjustment)){
        variance_factor <- 1
      } else if(inherits(x = variance_adjustment, what = "function")){
        adjustment_params <- names(formals(variance_adjustment))

        adjustment_param_list <- list()
        for(i in 1:length(adjustment_params)){
          adjustment_param_list[[adjustment_params[i]]] <-
            get(x = adjustment_params[i])
        }

        variance_factor <-
          do.call(
            what = variance_adjustment,
            args = adjustment_param_list
          )
      } else {
        stop("`variance_adjustment` must be NULL (i.e. no adjustment) or a ",
             "function that uses the results of `standardization_fit()`.")
      }

      if(se_method == "bootstrap"){
        boot_result <-
          do.call(
            what = boot::boot,
            args =
              c(
                list(
                  data = data,
                  statistic =
                    function(data, indices, ...) {
                      do.call(
                        what = standardization_fit,
                        args =
                          list(
                            data = data[indices,],
                            ...
                          )
                      )
                    },
                  outcome_formula_treatment = outcome_formula_treatment,
                  outcome_formula_control = outcome_formula_control,
                  outcome_formula = outcome_formula,
                  family = family,
                  predictors = predictors,
                  treatment_column = treatment_column,
                  estimand = estimand,
                  verbose = FALSE
                ),
                bootstrap_parameters
              )
          )

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
            outcome_formula_treatment = outcome_formula_treatment,
            outcome_formula_control = outcome_formula_control,
            family = family,
            treatment_column = treatment_column,
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
            se_method = "bootstrap",
            variance = sd(boot_result$t)^2,
            se_boot = sd(boot_result$t),
            se_boot_df_adjusted = sd(boot_result$t*sqrt(variance_factor)),
            t(do.call(what = c, args = ci_results)),
            estimand = estimand,
            variance_factor = variance_factor
          )

        if(verbose){
          return(
            list(
              result = boot_result_ci,
              boot_object = boot_result
            )
          )
        } else {
          return(boot_result_ci)
        }

      } else if(se_method %in% c("influence", "score")){
        n_observations <- standardization_result$n_observations

        if(is.null(outcome_formula)){
          if(
            !all(
              attributes(terms(outcome_formula_treatment))$intercept == 1,
              attributes(terms(outcome_formula_control))$intercept == 1
            )
          ){
            stop("Influence function not implemented for models without an ",
                 "intercept.")
          }
        } else {
          if(!attributes(terms(outcome_formula))$intercept){
            stop("Influence function not implemented for models without an ",
                 "intercept.")
          }
        }

        influence <-
          do.call(
            what = influence_function,
            args =
              standardization_result[
                intersect(
                  x = names(formals(influence_function)),
                  y = names(standardization_result)
                )
              ]
          )

        variance_eif <- sum(influence^2)
        se_eif <- sqrt(variance_eif)

        if(se_method == "influence"){
          se <- se_eif
        } else if(se_method == "score") {
          se <- se_eif*(1 - qnorm(p = 1 - alpha/2)/n_observations)^(-1/2)
        }

        variance_eif <- se_eif^2
        variance_eif_adjusted <- variance_eif*variance_factor

        standardization_result <-
          stats::setNames(
            object =
              c(standardization_result$estimate, se,
                standardization_result$estimate +
                  c(-1, 1)*qnorm(p = c(1 - alpha/2))*se),
            nm = c("estimate", paste0("se_", se_method),
                   paste0(c("lcl", "ucl"), "_", se_method, "_", 1 - alpha))

          ) |> t() |>
          data.frame(
            estimand = estimand
          )

        standardization_result$variance_factor <- variance_factor

        return(standardization_result)
      }
    }
  }




#' @rdname standardization
#' @param predictors A \code{character} vector containing the names of
#' covariates: used to check for missing values to be imputed.
#' @export

standardization_fit <-
  function(
    data,
    outcome_formula = NULL,
    outcome_formula_treatment = NULL,
    outcome_formula_control = NULL,
    family,
    treatment_column,
    predictors,
    estimand,
    verbose
  ){

    # Impute Missing Data
    if(any(is.na(data[, predictors]))){
      # Impute any missing values using mean/mode imputation
      data <-
        impute_covariates_mean_mode(
          data = data,
          baseline_covariates = predictors
        )
    }

    # Fit Regression Model(s) & Generate Counterfactual Predictions
    if(is.null(outcome_formula)){
      outcome_column <-
        all.vars(
          expr = update.formula(old = outcome_formula_treatment, new = . ~ 1)
        )

      a1r1 <-
        eval(
          expr =
            parse(
              text = paste0("(", treatment_column, " == 1 ) & !is.na(",
                            outcome_column, ")")
              ),
          envir = data
        )


      a0r1 <-
        eval(
          expr =
            parse(
              text = paste0("(", treatment_column, " == 0 ) & !is.na(",
                            outcome_column, ")")
            ),
          envir = data
        )

      y1_model <-
        stats::glm(
          formula = outcome_formula_treatment,
          family = family,
          data = data[which(a1r1),]
        )

      y0_model <-
        stats::glm(
          formula = outcome_formula_treatment,
          family = family,
          data = data[which(a0r1),]
        )

      y1_pred <-
        stats::predict(
          object = y1_model,
          newdata = data,
          type = "response"
        )

      y0_pred <-
        stats::predict(
          object = y0_model,
          newdata = data,
          type = "response"
        )
    } else {
      outcome_model <-
        stats::glm(
          formula = outcome_formula,
          family = family,
          data = data
        )

      outcome_column <-
        all.vars(expr = update.formula(old = outcome_formula, new = . ~ 1))

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
    }

    # Compute Estimate
    if(estimand == "difference"){
      estimate = mean(y1_pred) - mean(y0_pred)
    } else if(estimand == "ratio"){
      estimate = mean(y1_pred)/mean(y0_pred)

    } else if(estimand == "oddsratio"){
      estimate <-
        (mean(y1_pred)/(1 - mean(y1_pred))) /
        (mean(y0_pred)/(1 - mean(y0_pred)))
    }

    if(verbose){
      if(is.null(outcome_formula)){
        n_observations_treatment <- length(y1_model$residuals)
        n_observations_control <- length(y0_model$residuals)

        n_observations <- n_observations_treatment + n_observations_control

        coefficient_names_tx <- names(coefficients(y1_model))
        has_intercept_treatment <- "(Intercept)" %in% coefficient_names_tx

        coefficient_names_control <- names(coefficients(y0_model))
        has_intercept_control <- "(Intercept)" %in% coefficient_names_control

        n_parameters_treatment <-
          setdiff(
            x = names(coefficients(y1_model)),
            y = "(Intercept)"
          ) |>
          length()

        n_parameters_control <-
          setdiff(
            x = names(coefficients(y0_model)),
            y = "(Intercept)"
          ) |>
          length()

        # Implement influence function for stratified models
        influence <- NULL

        return(
          list(
            estimate = estimate,
            estimand = estimand,
            model_type = "stratified",
            outcome_model_treatment = y1_model,
            outcome_model_control = y0_model,
            influence = influence,
            y = data[, outcome_column],
            a = data[, treatment_column],
            y1_pred = y1_pred,
            y0_pred = y0_pred,
            n_observations_treatment = n_observations_treatment,
            n_observations_control = n_observations_control,
            n_observations = n_observations,
            has_intercept_treatment = has_intercept_treatment,
            has_intercept_control = has_intercept_control
          )
        )

      } else {
        a <- data[, treatment_column]
        y <- data[, outcome_column]

        y_pred <-
          stats::predict(
            object = outcome_model,
            newdata = data
          )

        n_observations <- length(outcome_model$residuals)
        coefficient_names <- names(coefficients(outcome_model))
        has_intercept <- "(Intercept)" %in% coefficient_names

        coefs <-
          setdiff(
            x = names(coefficients(outcome_model)),
            y = "(Intercept)"
          )

        n_parameters <- length(coefs)

        tx_terms <-
          paste(
            c(paste0("\\:", treatment_column),
              paste0(treatment_column, "\\:")
            ),
            collapse = "|"
          )

        n_parameters_treatment_interaction <-
          length(grep(pattern = tx_terms, x = coefs))

        n_parameters_tx <-
          n_parameters_treatment_interaction + (treatment_column %in% coefs)

        if(n_parameters_treatment_interaction == 0){

          influence <-
            2*(2*a - 1)*(y - y_pred) -
            ((y1_pred - mean(y1_pred)) - (y0_pred - mean(y0_pred)))

          influence[which(is.na(influence))] <- 0
        } else {
          influence <- NULL
        }

        return(
          list(
            estimate = estimate,
            estimand = estimand,
            influence = influence,
            outcome_model = outcome_model,
            model_type = "single",
            y = y,
            a = a,
            y_pred = y_pred,
            y1_pred = y1_pred,
            y0_pred = y0_pred,
            n_observations = n_observations,
            has_intercept = has_intercept,
            has_treatment_interaction = n_parameters_treatment_interaction > 0
          )
        )
      }
    } else {
      return(estimate)
    }
  }


#' @rdname standardization
#' @param y A \code{numeric} vector containing the outcome values.
#' @param a A \code{numeric} vector containing the binary treatment assignment
#' indicator.
#' @param y_pred A \code{numeric} vector containing the model-predicted value
#' of the outcome under the observed treatment assignment.
#' @param y1_pred A \code{numeric} vector containing the model-predicted value
#' of the outcome under assignment to treatment.
#' @param y0_pred A \code{numeric} vector containing the model-predicted value
#' of the outcome under assignment to control.
#' @param model_type A \code{character} scalar: either "stratified" or "single"
#' model, used to indicate the analysis approach
#' @param has_intercept A \code{logical} scalar, indicating whether an intercept
#' term is included in the model.
#' @param has_treatment_interaction A \code{logical} scalar, indicating whether
#' treatment-by-covariate interactions are included in the model.
#' @export

standardization_influence <-
  function(
    y = NULL,
    a = NULL,
    y_pred = NULL,
    y1_pred = NULL,
    y0_pred = NULL,
    estimand,
    model_type,
    has_intercept,
    has_treatment_interaction
  ){

    if(estimand == "difference"){
      a_bar <- mean(a[which(!is.na(y))])
      n_1 <- sum(a[which(!is.na(y))])
      n_0 <- sum((1 - a)[which(!is.na(y))])
      n <- n_1 + n_0
      y1_bar <- mean(y[which(a == 1 & !is.na(y))])
      y0_bar <- mean(y[which(a == 0 & !is.na(y))])
      f1_bar <- sum(a*y1_pred)/n_1
      f0_bar <- sum((1 - a)*y0_pred)/n_0

      beta_hat <- mean(y1_pred) - mean(y0_pred)

      influence <-
        ((a/n_1) - (1-a)/n_0)*y - beta_hat/n -
        (a - a_bar)*(y1_pred/n_1 + y0_pred/n_0) -
        (a - a_bar)*((y1_bar - f1_bar)/n_1 + (y0_bar - f0_bar)/n_0)

      influence[which(is.na(influence))] <- 0
      return(influence)

    } else {
      stop("Influence function not yet implemented for estimand \"",
           estimand, "\"")
    }
  }




#' @rdname standardization
#' @export

standardization_df_adjust_tsiatis_2008 <-
  function(
    data,
    outcome_formula = NULL,
    outcome_formula_treatment = NULL,
    outcome_formula_control = NULL,
    estimand,
    treatment_column
  ){
    if(
      !xor(
        x = inherits(x = outcome_formula, what = "formula"),
        y = all(inherits(x = outcome_formula_treatment, what = "formula"),
                inherits(x = outcome_formula_control, what = "formula"))
      )
    ){
      stop("Either `outcome_formula` must be a formula, or both ",
           "`outcome_formula_treatment` and `outcome_formula_control` must ",
           "be formulas.")
    }

    if(!estimand %in% c("difference", "ratio", "oddsratio")){
      stop("Degree-of-freedom adjustment not yet implemented for estimand \"",
           estimand, "\"")
    }

    if(is.null(outcome_formula)){
      if(estimand == "difference"){
        model_terms_1 <- terms(x = outcome_formula_treatment)
        model_terms_0 <- terms(x = outcome_formula_control)

        treatment_model_vars <-
          terms(x = outcome_formula_treatment) |>
          all.vars()

        n_1 <-
          subset(
            x = data,
            subset = eval(expr = parse(text = paste0(treatment_column, " == 1"))),
            select = treatment_model_vars
          ) |>
          complete.cases() |>
          sum()

        control_model_vars <-
          terms(x = outcome_formula_control) |>
          all.vars()

        n_0 <-
          subset(
            x = data,
            subset = eval(expr = parse(text = paste0(treatment_column, " == 0"))),
            select = control_model_vars
          ) |>
          complete.cases() |>
          sum()

        coefs_1 <- colnames(model.matrix(object = model_terms_1, data = data))
        p_1 <- sum(coefs_1 != "(Intercept)")

        coefs_0 <- colnames(model.matrix(object = model_terms_0, data = data))
        p_0 <- sum(coefs_0 != "(Intercept)")

        return(
          (1/(n_0 - p_0 - 1) + 1/(n_1 - p_1 - 1))/(1/(n_0 - 1) + 1/(n_1 - 1))
        )
      } else {
        stop("Correction not available for estimand \"", estimand, "\"")
      }
    } else {
      if(estimand == "difference"){
        model_terms <- terms(x = outcome_formula)
        n <-
          sum(complete.cases(data[, rownames(attributes(model_terms)$factors)]))

        all_coefs <- colnames(model.matrix(object = model_terms, data = data))

        p <- sum(all_coefs != "(Intercept)")

        tx_terms <-
          paste(
            c(paste0("\\:", treatment_column),
              paste0(treatment_column, "\\:")
            ),
            collapse = "|"
          )

        treatment_interactions <-
          length(grep(pattern = tx_terms, x = all_coefs))

        if(treatment_interactions == 0){
          return((n - 1)/(n - p - 1))
        } else {
          stop("Correction not available with treatment-covariate interactions.")
        }
      } else {
        stop("Correction not available for estimand \"", estimand, "\"")
      }
    }
  }
