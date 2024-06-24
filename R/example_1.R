#' Example 1: Simulated Trial with a Single, Continuous Outcome
#'
#' Simulated data from a randomized trial with a single, continuous
#' outcome obtained post-randomization and continuous baseline covariates.
#'
#' @format ## `example_1`
#' A data frame with 368 rows and 11 columns:
#' \describe{
#'   \item{id}{Participant ID}
#'   \item{treatment}{Treatment Assigned}
#'   \item{enrollment_time}{Time from study initiation to randomization}
#'   \item{x1, x2, x3, x4, x5}{Baseline Covariates}
#'   \item{follow_up_time}{Time from study initiation to last study visit}
#'   \item{tx}{Binary treatment assignment (1 = Treatment; 0 = Control)}
#'   \item{y}{Primary outcome: higher values indicate better outcomes}
#' }
"example_1"
