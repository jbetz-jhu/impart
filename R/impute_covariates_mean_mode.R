#' Impute missing baseline covariates using mean/mode imputation
#'
#' This function is for impute missing values in baseline covariates. For
#' \code{numeric} columns, the mean value of the observed values is imputed.
#' For \code{factor} columns, the mode value of the observed values is imputed.
#'
#' @param data a \code{data.frame} to impute
#' @param baseline_covariates A \code{character} \code{vector} containing the
#' names in \code{data} to impute.
#' @param binary_categorical A \code{logical} scalar indicating whether binary
#' variables (logicals, numeric columns containing only
#' \code{0}/\code{1}/\code{NA}) should be imputed as a categorical variable
#' (using the mode) or a continuous variable (using the mean). The latter
#' preserves the type of the column in output.
#'
#' @return A \code{data.frame} after imputing the columns named in
#' \code{baseline_covariates}
#'
#' @export
#'
#' @references {
#' Benkeser, D, Díaz, I, Luedtke, A, Segal, J, Scharfstein, D, and Rosenblum,
#' M. 2020. "Improving Precision and Power in Randomized Trials for COVID-19
#' Treatments Using Covariate Adjustment, for Binary, Ordinal, and Time-to-Event
#' Outcomes." \emph{Biometrics} 77 (4): 1467–81.
#' \url{https://doi.org/10.1111/biom.13377}.
#' }
#'
#' @examples
#' set.seed(12345)
#' mtcars_missing <- mtcars
#' mtcars_missing$cyl <- factor(mtcars_missing$cyl)
#' mtcars_missing$vs <- factor(mtcars_missing$vs)
#' mtcars_missing$am <- factor(mtcars_missing$am)
#' mtcars_missing$gear <- factor(mtcars_missing$gear)
#' for(i in 1:ncol(mtcars_missing))
#'   mtcars_missing[sample(x = 1:nrow(mtcars_missing), size = 3), i] <- NA
#'
#' impute_covariates_mean_mode(
#'   data = mtcars_missing,
#'   baseline_covariates = names(mtcars_missing)
#' )

impute_covariates_mean_mode <-
  function(
    data,
    baseline_covariates,
    binary_categorical = TRUE
  ) {

    if(length(baseline_covariates) > 0){

      if(!all(baseline_covariates %in% names(data))){
        missing_covariates <-
          setdiff(
            x = baseline_covariates,
            y = names(data)
          )
        stop('`baseline_covariates` not found: ',
             paste(missing_covariates, collapse = ", "))
      }

      allowed_classes <-
        c("numeric", "integer", "factor", "string",
          "logical", "character", "complex")

      if(binary_categorical){
        class_mean <- c("numeric", "integer", "complex")
        class_mode <- c("logical", "factor", "character", "binary")
      } else {
        class_mean <- c("numeric", "integer", "complex", "binary", "logical")
        class_mode <- c("factor", "character")
      }


      Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }

      impute_mean <- function(x) {
        replace(
          x = x,
          list = is.na(x),
          values = mean(x, na.rm = TRUE)
        )
      }

      impute_mode <- function(x) {
        replace(
          x = x,
          list = is.na(x),
          values = Mode(x)
        )
      }

      var_class <-
        sapply(
          X = data[baseline_covariates],
          FUN = function(x, allowed = allowed_classes) {
            allowed[which(allowed %in% class(x))[1]]
          }
        )

      is_binary <-
        sapply(
          X = data[baseline_covariates],
          FUN = function(x) all(x %in% c(0L, 1L, NA_integer_))
        )

      if(any(is_binary)) var_class[which(is_binary)] <- "binary"

      var_class <-
        replace(
          x = var_class,
          list = var_class %in% class_mean,
          values = "mean"
        )

      var_class <-
        replace(
          x = var_class,
          list = var_class %in% class_mode,
          values = "mode"
        )

      unable_to_class <- which(!var_class %in% c("mean", "mode"))
      if(length(unable_to_class) > 0){
        stop("Unable to determine class of variables: ",
             paste(names(var_class)[unable_to_class], collapse = ", "))
      }

      for(i in names(which(var_class == "mean"))){
        data[, i] <- impute_mean(data[, i])
      }

      for(i in names(which(var_class == "mode"))){
        data[, i] <- impute_mode(data[, i])
      }

      return(data)
    } else {
      return(data)
    }
  }
