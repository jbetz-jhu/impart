#' Example 1: Simulated Trial with a Single, Continuous Outcome
#'
#' Simulated data from a randomized trial with four continuous outcomes obtained
#' at 30, 60, 90, and 120 days post-randomization and continuous baseline
#' covariates.
#'
#' @format ## `example_1`
#' A data frame with 300 rows and 15 columns:
#' \describe{
#'   \item{.id}{Participant ID}
#'   \item{x_1, x_2, x_3, x_4}{Baseline Covariates 1-4}
#'   \item{tx}{Binary treatment assignment (1 = Treatment; 0 = Control)}
#'   \item{y_1,y_2,y_3,y_4}{Outcomes at assessments 1-4}
#'   \item{.enrollment_time}{Time from study initiation to randomization}
#'   \item{.t_1,.t_2,.t_3,.t_4}{Study time of assessments 1-4}
#' }
"example_1"




#' Example 1: Simulated Trial with a Single, Continuous Outcome - Interim Analysis 1
#'
#' Simulated data from a randomized trial with four continuous outcomes obtained
#' at 30, 60, 90, and 120 days post-randomization and continuous baseline
#' covariates. This dataset contains the data observed by 788 days since study
#' initiation.
#'
#' @format ## `example_1_ia_1`
#' A data frame with 134 rows and 15 columns:
#' \describe{
#'   \item{.id}{Participant ID}
#'   \item{x_1, x_2, x_3, x_4}{Baseline Covariates 1-4}
#'   \item{tx}{Binary treatment assignment (1 = Treatment; 0 = Control)}
#'   \item{y_1, y_2, y_3, y_4}{Outcomes at assessments 1-4}
#'   \item{.enrollment_time}{Time from study initiation to randomization}
#'   \item{.t_1,.t_2,.t_3,.t_4}{Study time of assessments 1-4}
#' }
"example_1_ia_1"




#' Example 1: Simulated Trial with a Single, Continuous Outcome - Interim Analysis 2
#'
#' Simulated data from a randomized trial with four continuous outcomes obtained
#' at 30, 60, 90, and 120 days post-randomization and continuous baseline
#' covariates. This dataset contains the data observed by 1194 days since study
#' initiation.
#'
#' @format ## `example_1_ia_2`
#' A data frame with 198 rows and 15 columns:
#' \describe{
#'   \item{.id}{Participant ID}
#'   \item{x_1, x_2, x_3, x_4}{Baseline Covariates 1-4}
#'   \item{tx}{Binary treatment assignment (1 = Treatment; 0 = Control)}
#'   \item{y_1, y_2, y_3, y_4}{Outcomes at assessments 1-4}
#'   \item{.enrollment_time}{Time from study initiation to randomization}
#'   \item{.t_1,.t_2,.t_3,.t_4}{Study time of assessments 1-4}
#' }
"example_1_ia_2"





#' Example 1: Simulated Trial with a Single, Continuous Outcome - Final Analysis
#'
#' Simulated data from a randomized trial with four continuous outcomes obtained
#' at 30, 60, 90, and 120 days post-randomization and continuous baseline
#' covariates. This dataset contains the data observed by 1499 days since study
#' initiation.
#'
#' @format ## `example_1_final`
#' A data frame with 239 rows and 15 columns:
#' \describe{
#'   \item{.id}{Participant ID}
#'   \item{x_1, x_2, x_3, x_4}{Baseline Covariates 1-4}
#'   \item{tx}{Binary treatment assignment (1 = Treatment; 0 = Control)}
#'   \item{y_1, y_2, y_3, y_4}{Outcomes at assessments 1-4}
#'   \item{.enrollment_time}{Time from study initiation to randomization}
#'   \item{.t_1,.t_2,.t_3,.t_4}{Study time of assessments 1-4}
#' }
"example_1_final"




#' colon_cancer: Processed Moerton Colon Cancer Data
#'
#' Data from an adjuvant chemotherapy trial for colon cancer with variables
#' renamed and labeled, missing covariates imputed, and time scale converted
#' to years. For more information, see \code{?survival::colon}.
#'
#' @format ## `colon_cancer`
#' A data frame with 929 rows and 15 columns:
#' \describe{
#'   \item{.id}{Participant ID}
#'   \item{arm}{Treatment Assignment}
#'   \item{age}{Patient age}
#'   \item{sex}{Patient sex}
#'   \item{obstruction}{Colon obstruction by tumor}
#'   \item{perforation}{Colon perforation by tumor}
#'   \item{organ_adherence}{Organ adherence by tumor}
#'   \item{positive_nodes}{Lymph nodes with detectable cancer}
#'   \item{differentiation}{Differentiation of tumor}
#'   \item{local_spread}{Extent of local spread of tumor}
#'   \item{time_surgery_registration}{Time from surgery to registration}
#'   \item{event_death}{Death on study (1 = yes, 0 = no)}
#'   \item{event_recurrence}{Recurrence (1 = yes, 0 = no)}
#'   \item{years_to_death}{Time to death/censoring in years}
#'   \item{years_to_recurrence}{Time to recurrence/censoring in years}
#' }
"colon_cancer"
