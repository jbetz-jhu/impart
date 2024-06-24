#' Example 1: Simulated Trial with a Single, Continuous Outcome
#'
#' Simulated data from a randomized trial with four continuous outcomes obtained
#' at 30, 60, 90, and 120 days post-randomization.
#'
#'  and continuous baseline covariates.
#'
#' @format ## `example_1`
#' A data frame with 276 rows and 15 columns:
#' \describe{
#'   \item{.id}{Participant ID}
#'   \item{x_1, x_2, x_3, x_4}{Baseline Covariates 1-4}
#'   \item{tx}{Binary treatment assignment (1 = Treatment; 0 = Control)}
#'   \item{y_1, y_2, y_3, y_4} Outcomes at assessments 1-4
#'   \item{.enrollment_time}{Time from study initiation to randomization}
#'   \item{.t_1, .t_2, .t_3, .t_4} Outcomes at assessments 1-4
#' }
"example_1"
