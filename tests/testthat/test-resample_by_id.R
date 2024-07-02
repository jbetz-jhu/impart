# 1. Input Error Handling ######################################################
test_that(
  desc = "Check for `.id`, mismatched values, length of `ids_to_sample",
  code = {
    # 1. No `.id` Column
    expect_error(
      object =
        resample_by_id(
          data =
            data.frame(
              id = 1:10,
              arm = c(rep(1, 5), rep(0, 5)),
              y = 1:10
            ),
          ids_to_sample = sample(1:10, replace = TRUE)
        ),
      regexp = "`.id` column must be present"
    )

    # 2. Values in `ids_to_sample` do not match `.id` Column
    expect_error(
      object =
        resample_by_id(
          data =
            data.frame(
              .id = 1:10,
              arm = c(rep(1, 5), rep(0, 5)),
              y = 1:10
            ),
          ids_to_sample = c(sample(x = 1:7, replace = TRUE), 20, 22, NA)
        ),
      regexp = "Unmatched values of `.id`"
    )

    # 3. Number of unique values in `.id` doesn't match length of `ids_to_sample`
    expect_error(
      object =
        resample_by_id(
          data =
            data.frame(
              .id = 1:10,
              arm = c(rep(1, 5), rep(0, 5)),
              y = 1:10
            ),
          ids_to_sample = sample(1:4, replace = TRUE)
        ),
      regexp = "Number of unique ids"
    )
  }
)

# resample_by_id(
#   data =
#     data.frame(
#       .id = letters[1:10],
#       arm = c(rep(1, 5), rep(0, 5)),
#       y = 1:10
#     ),
#   ids_to_sample = letters[sample(1:10, replace = TRUE)]
# )
#
# x <-
#   resample_by_id(
#     data =
#       data.frame(
#         .id = letters[1:10],
#         arm = c(rep(1, 5), rep(0, 5)),
#         y = 1:10
#       ),
#     ids_to_sample = letters[sample(1:10, replace = TRUE)],
#     convert_id_to_factor = TRUE
#   )
