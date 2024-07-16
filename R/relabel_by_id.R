#' Create relabeled dataset from a list of resampled IDs
#'
#' This is an internal function used by [impart::calculate_covariance] for
#' bootstrapping data that may be either in wide format (i.e. one row per
#' individual) or long format (i.e. one row per study visit per individual).
#' This function takes in a list of resampled IDs, retrieves all rows of data
#' corresponding to these IDs, and creates a unique ID that preserves the
#' nesting structure of long format of data.
#'
#' The original data must have a column named `.id` that identifies which rows
#' of data belong to an individual. A new value of `.id` is created in the
#' resulting data, preserving the nesting structure in long data.
#'
#' @param data A \code{data.frame} to be resampled, which must contain a column
#' named `.id` which uniquely identifies all rows corresponding to an
#' individual.
#' @param ids a vector of IDs that have already been resampled from
#' \code{data}
#' @param resample Logical scalar: Should the IDs be resampled for computing a
#' bootstrap replicate from a list of unique IDs?
#' @param convert_id_to_factor Logical scalar: Should the ID column be converted
#' to a factor?
#'
#' @return A \code{data.frame} containing the resampled data with unique values
#' of `.id`.
#'
#' @export
#'
#' @examples
#' # To be added
#'

relabel_by_id <-
  function(
    data,
    ids,
    resample = FALSE,
    convert_id_to_factor = TRUE
  ){
    if(!".id" %in% names(data)){
      stop("`.id` column must be present in `data`.")
    }

    if(!all(ids %in% data$.id)){
      unmatched_ids <-
        setdiff(
          x = ids,
          y = data$.id
        )
      stop("Unmatched values of `.id` in `ids`: ",
           paste0("'", unmatched_ids, "'", collapse = ", "))
    }

    if(length(unique(data$.id)) != length(ids)){
      stop("Number of unique ids in dataset (", length(unique(data$.id)), ") ",
           "does not match the length of `ids` (",
           length(ids), ").")
    }

    if(resample) ids <- sample(x = ids, replace = TRUE)

    id_column <- which(names(data) == ".id")

    ids_new <-
      paste0(ids, ".",
             count =
               stats::ave(
                 x = ids == ids,
                 ids,
                 FUN = cumsum)
             )

    if(convert_id_to_factor){
      ids_new <- factor(x = ids_new)
    }

    new_data <-
      lapply(
        X = 1:length(ids),
        FUN =
          function(
    x,
    id_o = ids,
    id_n = ids_new,
    id_all = data$.id
          ){
            data.frame(
              `.id` = id_n[x],
              `.id_source` = id_o[x],
              rows = which(id_all %in% id_o[x])
            )
          }
      ) |>
      do.call(
        what = rbind
      )

    return(
      data.frame(
        new_data[, c(".id", ".id_source")],
        data[new_data$rows, -id_column]
      )
    )
  }
