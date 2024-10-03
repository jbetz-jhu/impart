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




#' test_data: A dataset used for testing package functions
#'
#' This data.frame contains two time to event outcomes and two numeric outcomes,
#' and is used for internal testing purposes. Enrollment time starts at day 0,
#' with two individuals enrolled per day. The event time for the first event
#' is the ceiling of the participant ID divided by 2. The event time for the
#' second event is the participant ID multiplied by 2. The outcome time for the
#' numeric outcome is equal to the participant ID, and the second outcome time
#' is always 5 greater than the first outcome time.
#'
#' @format ## `test_data`
#' A data frame with 10 rows and 12 columns:
#' \describe{
#'   \item{.id}{Participant ID}
#'   \item{enrollment}{Entry time}
#'   \item{x_1}{A binary covariate}
#'   \item{tx}{Binary treatment assignment}
#'   \item{tte_1}{time to `event_1`}
#'   \item{tte_2}{time to `event_2`}
#'   \item{event_1}{Event 1 indicator}
#'   \item{event_2}{Event 2 indicator}
#'   \item{y_1}{Intermediate outcome - 1 of 2}
#'   \item{y_2}{Final Outcome - 2 of 2}
#'   \item{.t_1}{Time of intermediate outcome}
#'   \item{.t_2}{Time of final outcome}
#' }
"test_data"
