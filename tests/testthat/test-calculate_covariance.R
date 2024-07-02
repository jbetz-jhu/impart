#
# set.seed(12345)
# test_data <-
#   data.frame(
#     id = 1:100,
#     y = rnorm(n = 100),
#     group = ceiling(1:100/25)
#   )
#
# get_lm_intercept <-
#   function(
#     data, formula
#   ){
#     return(as.numeric(coef(lm(formula = formula, data = data))["(Intercept)"]))
#   }
#
# test_that("multiplication works", {
#   expect_equal(
#     calculate_covariance(
#       data =
#         subset(x = test_data, group %in% 1),
#       ids_by_analysis = NULL,
#       bootstrap_ids = NULL,
#       bootstrap_results = NULL,
#       estimation_function = get_lm_intercept,
#       estimation_arguments =
#         list(
#           formula = y ~ 1
#         ),
#       rng_seed = 23456,
#       control = monitored_analysis_control()
#     ),
#     4)
# })
