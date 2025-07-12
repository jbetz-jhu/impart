test_that(
  desc = "Error Handling",
  code = {
    expect_error(
      object =
        impute_covariates_mean_mode(
          data = mtcars,
          baseline_covariates = "nonexistant_covariate",
          binary_categorical = TRUE
        ),
      regex = "`baseline_covariates` not found"
    )
  }
)




# Test against numeric, integer, complex, logical, factor, character
test_that(
  desc = "Imputation Works for Usual Classes",
  code = {
    set.seed(123456)
    factor_levels <- paste("Level", 1:3)
    test_data_rows <- 20
    n_missing <- 4

    test_data <-
      data.frame(
        x_numeric = sample(x = 1:10, size = test_data_rows, replace = TRUE) |>
          as.numeric(),
        x_binary_numeric =
          sample(x = c(0:1), size = test_data_rows, replace = TRUE) |>
          as.numeric(),
        x_binary_integer =
          sample(x = c(0L, 1L), size = test_data_rows, replace = TRUE),
        x_integer = sample(x = 1L:10L, size = test_data_rows, replace = TRUE),
        x_factor =
          factor(
            x = sample(x = factor_levels, size = test_data_rows, replace = TRUE),
            levels = factor_levels
          ),
        x_logical =
          sample(x = c(FALSE, TRUE), size = test_data_rows, replace = TRUE),
        x_character =
          sample(x = c("A", "B", "C"), size = test_data_rows, replace = TRUE),
        x_complex =
          sample(x = 1:10, size = test_data_rows, replace = TRUE) + 1i
      )

    original_classes <- sapply(X = test_data, FUN = class)

    test_data_mcar <- test_data

    for(i in 1:ncol(test_data)){
      test_data_mcar[sample(x = 1:test_data_rows, size = n_missing), i] <- NA
    }

    expect_no_condition(
      object =
        impute_covariates_mean_mode(
          data = test_data_mcar,
          baseline_covariates = names(test_data),
          binary_categorical = TRUE
        )
    )

    expect_no_condition(
      object =
        impute_covariates_mean_mode(
          data = test_data_mcar,
          baseline_covariates = names(test_data),
          binary_categorical = FALSE
        )
    )

    # For types other than integer, result should be same class
    expect_identical(
      object =
        sapply(
          X =
            impute_covariates_mean_mode(
              data = test_data_mcar[, -4],
              baseline_covariates = names(test_data)[-4],
              binary_categorical = TRUE
            ),
          FUN = class
        ),
      expected = sapply(X = test_data[, -4], FUN = class)
    )

    # If no variables specified, original dataset returned
    expect_identical(
      object =
        impute_covariates_mean_mode(
          data = test_data_mcar,
          baseline_covariates = character(0),
          binary_categorical = TRUE
        ),
      expected = test_data_mcar
    )
  }
)
