independent_mvn <-
  function(
    n,
    mean_x = NULL,
    sigma_x
  ) {
    if(!is.matrix(sigma_x)) sigma_x <- diag(x = sigma_x)
    dim_x <- ncol(sigma_x)
    if(is.null(mean_x)) mean_x <- rep(0, dim_x)

    setNames(
      object =
        data.frame(
          mvtnorm::rmvnorm(
            n = n,
            mean = mean_x,
            sigma = sigma_x
          )
        ),
      nm = paste0("x_", 1:dim_x)
    )
  }




rnd_block <-
  function(
    data = data,
    strata = NULL,
    block_sizes = c(1, 2, 3),
    treatment_labels = c(0, 1)
  ) {

    n_treatments <- length(treatment_labels)
    n_obs = nrow(data)

    stopifnot(
      is.null(strata)|sum(is.na(strata$stratum)) == 0
    )

    n_strata <-
      ifelse(
        test = is.null(strata),
        yes = 1,
        no = length(unique(strata$stratum))
      )

    tx <- matrix(data = NA, nrow = n_obs, ncol = n_strata)

    for(i in 1:n_strata){
      blocks <-
        sample(
          x = n_treatments*block_sizes,
          size = n_obs/min(block_sizes),
          replace = TRUE
        )

      tx_i <-
        sapply(
          X = as.list(blocks),
          FUN = function(x) sample(rep(x = treatment_labels, each = x))
        )[1:n_obs]

      tx[, i] <- head(x = do.call(what = c, args = tx_i), n_obs)
    }

    if(n_strata == 1){
      data$tx <- tx[, 1]
    } else{
      data$tx <- tx[cbind(1:n_obs, data$stratum)]
    }

    return(data)
  }



outcome_glm <-
  function(
    model = NULL,
    params = NULL,
    family = NULL,
    data = NULL
  ){
    model_constructed <- is.null(model)

    # If parameters are passed to construct a "model":
    if(model_constructed){
      outcome_var <- all.vars(update(params$formula, . ~ 0))

      if(!outcome_var %in% names(data)) data[, outcome_var] <-
          runif(n = nrow(data))

      # Construct a GLM model using formula, data, and supplied parameters
      model_frame_qr <-
        base::qr(
          stats::model.frame(
            formula = params$formula,
            data = data
          )
        )

      model =
        list(
          coefficients = params$coefficients,
          rank = model_frame_qr$rank,
          family =
            switch(
              EXPR = class(family),
              "function" = family(),
              "family" = family
            ),
          model =
            model.frame(
              formula = params$formula,
              data = data
            ),
          data = data,
          formula = params$formula,
          terms = terms(params$formula),
          qr = model_frame_qr
        )

      class(model) <- c("glm", "lm")
    }


    # Determine parameterization from model passed as argument

    model_type <-
      switch(
        EXPR = paste(class(model), collapse = "|"),
        "glm|lm" = "stats::glm",
        "gam|glm|lm" = "mgcv::gam",
        "survreg" = "survival::survreg",
        "unknown"
      )

    if(model_type == "unknown"){
      stop("Unknown model type: use stat::glm, mgcv::gam, or survival::survreg")
    }

    if(model_type %in% c("stats::glm", "mgcv::gam")){
      if(model$family$family %in%
         c("gaussian", "binomial", "poisson", "Gamma")){
        outcome_glm_gam_fit(
          model = model,
          data = data,
          params = params,
          model_constructed = model_constructed
        )
      } else {
        stop("Family not implemented.")
      }
    } else if(model_type %in% c("survreg"))
      outcome_survreg_fit(
        model = model,
        data = data
      )
  }



outcome_glm_gam_fit <-
  function(
    model,
    params = NULL,
    data = NULL,
    model_constructed = FALSE
  ){
    model_type <-
      switch(
        EXPR = paste(class(model), collapse = "|"),
        "glm|lm" = "stats::glm",
        "gam|glm|lm" = "mgcv::gam"
      )

    if(is.null(data)) data <- model$data

    family = model$family$family

    outcome_var <- all.vars(update(model$formula, . ~ 0))

    prediction_interval <-
      predict(
        object = model,
        newdata = data,
        type = "response",
        se.fit = (family %in% c("gaussian") & !model_constructed)
      )

    if(family == "gaussian"){
      if(model_constructed){
        prediction_interval <-
          list(
            fit = prediction_interval,
            se.fit = 0*prediction_interval,
            residual.scale = params$residual_sd
          )
      } else{
        if(is.null(prediction_interval$residual.scale)){
          prediction_interval$residual.scale <- sigma(model)
        }
      }

      prediction_interval$sd <-
        (with(prediction_interval, sqrt(se.fit^2 + residual.scale^2)))

      data[, outcome_var] <-
        with(prediction_interval,
             rnorm(
               n = length(fit),
               mean = fit,
               sd = sd
             )
        )
    } else if(family == "binomial"){
      data[, outcome_var] <-
        rbinom(
          n = length(prediction_interval),
          size = 1,
          prob = prediction_interval
        )
    } else if(family == "poisson"){
      data[, outcome_var] <-
        rpois(
          n = length(prediction_interval),
          lambda = prediction_interval
        )
    } else{
      stop(paste0("Family `", family, "` not implemented."))
    }

    return(data)
  }


simulate_normal_glm_trial <-
  function(
    n_participants,
    n_covariates,
    r_squared_covariates,
    marginal_variance_y,
    mean_outcome = 0,
    tx_coefficient = 0
  ) {

    var_resid <- (1 - r_squared_covariates)*marginal_variance_y
    sigma_x <- diag(x = 1, nrow = n_covariates)

    beta_outcome <- rnorm(n = n_covariates)

    beta_outcome <-
      beta_outcome*
      sqrt((marginal_variance_y - var_resid)/
             as.numeric(t(beta_outcome)%*%sigma_x%*%beta_outcome))


    beta_outcome <- c(mean_outcome, tx_coefficient, beta_outcome)

    names(beta_outcome) <- c("(Intercept)", "tx", paste0("x_", 1:n_covariates))

    data =
      rnd_block(
        data =
          independent_mvn(
            n = n_participants,
            mean_x = rep(0, n_covariates),
            sigma_x = sigma_x
          )
      )

    outcome_formula <-
      paste("y ~ ", paste(c("tx", paste0("x_", 1:n_covariates)), collapse = " + "),
            collapse = "") |>
      as.formula()

    data <-
      outcome_glm(
        params =
          list(
            formula = outcome_formula,
            coefficients = beta_outcome,
            residual_sd = sqrt(var_resid)
          ),
        family = gaussian,
        data = data
      )

    return(data)
  }



sim_lme_trial <-
  function(
    data,
    visit_times,
    mean_outcomes,
    outcome_cov = NULL,
    re_variance,
    re_correlation = NULL,
    residual_sd,
    pr_dropout = NULL,
    dropout_cov = NULL
  ) {

    n_random_effects <-
      nrow(as.matrix(re_variance))

    stopifnot(length(visit_times) == length(mean_outcomes))

    if(n_random_effects == 1) {
      # Only random intercept is supplied
      re_covariance <- re_variance
    } else if(n_random_effects == 2) {
      # Slope/intercept supplied
      if(is.null(re_correlation)) { # Uncorrelated random effects
        re_correlation <- diag(n_random_effects)
      } else if (nrow(as.matrix(re_correlation)) == 1) { # Scalar correlation
        re_correlation <-
          diag(2) +
          re_correlation*(matrix(data = 1, nrow = 2, ncol = 2) - diag(2))
      }

      re_covariance <-
        (matrix(sqrt(re_variance), ncol = 1) %*%
           matrix(sqrt(re_variance), nrow = 1))*re_correlation

    } else {
      stop(paste0("Only random intercept, random slope, or random intercept ",
                  "and slope are implemented."))
    }


    n_outcomes <- length(x = visit_times)
    n_obs <- nrow(data)

    study_data <-
      data.frame(
        data,
        last_observed = NA,
        dropout_i = NA
      )

    # 1. Determine Distribution of outcomes

    # 1.1 Determine Fixed Effects
    out_cov_lp <- matrix(0, nrow = n_obs, ncol = n_outcomes)

    if(!is.null(outcome_cov)){
      for(i in 1:n_outcomes) {
        if(!is.null(outcome_cov[[i]])) {
          out_cov_lp[, i] <-
            as.matrix(study_data[, names(outcome_cov[[i]])]) %*%
            outcome_cov[[i]]
        }
      }
    }

    # 1.2 Simulate Random effects are simulated
    # Sample random effects
    random_effects <-
      mvtnorm::rmvnorm(
        n = n_obs,
        sigma = as.matrix(re_covariance)
      )

    # Create (n x T) matrix of disturbances due to random effects
    if(ncol(random_effects) == 1){ # Random Intercept
      random_trajectories <-
        kronecker(random_effects,
                  matrix(1, ncol = length(visit_times)))
      colnames(random_effects) <- c("random_intercept")
    } else { # Centered Random Effects at Mean Visit Time
      random_trajectories <-
        random_effects %*%
        rbind(1, visit_times - mean(visit_times))
      colnames(random_effects) <- c("random_intercept", "random_slope")
    }

    # 1.3. Add fixed effects and random effects to residual
    residuals <-
      mvtnorm::rmvnorm(
        n = n_obs,
        mean = matrix(0, ncol = n_outcomes),
        sigma = diag(residual_sd^2, nrow = n_outcomes)
      )
    colnames(residuals) <- paste0("residual_", 1:n_outcomes)

    study_data[, paste0("y_", 1:n_outcomes)] <-
      study_data[, paste0("y_obs_", 1:n_outcomes)] <-
      # Note: In order to make the dropout model easier to use, the mean
      # outcome is added in later.
      matrix(data = 0, nrow = n_obs, ncol = n_outcomes) +
      out_cov_lp + # Linear Predictor
      random_trajectories + # Random Effects
      residuals

    study_data <-
      data.frame(study_data, random_effects, residuals)

    # 2. dropout is assigned
    for(i in 1:n_outcomes) {

      if(is.null(pr_dropout[[i]])){
      } else if(pr_dropout[[i]] > 0 & pr_dropout[[i]] < 1) {

        if(is.null(dropout_cov[[i]])) {
          drop_cov_lp <- qlogis(1 - pr_dropout[[i]])
        } else {
          drop_cov_lp <- qlogis(1 - pr_dropout[[i]]) -
            as.matrix(study_data[names(dropout_cov[[i]])]) %*%
            dropout_cov[[i]]
        }

        study_data$dropout_i <-
          rlogis(n = n_obs, location = drop_cov_lp, scale = 1) < 0

        dropout.rows <-
          with(study_data, which(is.na(last_observed) & dropout_i))

        study_data[dropout.rows,
                   paste0("y_obs_", i:n_outcomes)] <- NA

        study_data$last_observed[dropout.rows] <- (i - 1)
      }
    }

    study_data$last_observed[which(is.na(study_data$last_observed))] <-
      n_outcomes

    study_data$dropout_i <- NULL

    # 3. Add in mean outcome
    study_data[, paste0("y_", 1:n_outcomes)] <-
      study_data[, paste0("y_", 1:n_outcomes)] +
      matrix(data = mean_outcomes, nrow = n_obs, ncol = n_outcomes,
             byrow = TRUE)

    study_data[, paste0("y_obs_", 1:n_outcomes)] <-
      study_data[, paste0("y_obs_", 1:n_outcomes)] +
      matrix(data = mean_outcomes, nrow = n_obs, ncol = n_outcomes,
             byrow = TRUE)

    study_data
  }
