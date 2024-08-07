% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardization.R
\name{standardization}
\alias{standardization}
\alias{standardization_correction}
\title{Compute stanadardization (i.e. G-Computation) estimator}
\usage{
standardization(
  data,
  estimand = "difference",
  y0_formula,
  y1_formula,
  family,
  treatment_column = NULL,
  outcome_indicator_column = NULL
)

standardization_correction(
  data,
  y0_formula,
  y1_formula,
  treatment_column,
  outcome_indicator_column
)
}
\arguments{
\item{data}{A \code{data.frame} containing baseline covariates (e.g. \code{x1},
\code{x2}, ...), a binary treatment indicator (e.g. \code{tx} where 1 = Treatment;
0 = Control), outcome variables (e.g. \code{y1}, \code{y2}, ...), and outcome
indicators (e.g. \code{.r_1}, \code{.r_2}, ...). The outcome indicators indicate
whether an outcome has been observed (\code{1}), is missing (\code{0}), or not yet
obtained (\code{NA}).}

\item{estimand}{A \code{character} scalar: "difference" (for a difference in
means or risk difference), "ratio" (for a ratio of means or relative risk),
or "oddsratio" (for an odds ratio for a binary outcome).}

\item{y0_formula}{A \link[stats:formula]{stats::formula} specifying the relationship between the
outcome and covariates in the control arm.}

\item{y1_formula}{A \link[stats:formula]{stats::formula} specifying the relationship between the
outcome and covariates in the treatment arm.}

\item{family}{The \link[stats:family]{stats::family} for the outcome regression model}

\item{treatment_column}{A \code{character} scalar indicating the column
containing the treatment indicator.}

\item{outcome_indicator_column}{A \code{character} scalar indicating the column
containing the outcome indicator corresponding to \code{treatment_column}.}
}
\value{
A \code{list} containing the marginal means and their contrast:
}
\description{
Computes the estimate of a contrast of means for continuous and binary
outcomes on the additive scale, relative scale. Computing a marginal odds
ratio is also possible for binary outcomes.
}
\details{
Generalized linear models, stratified by treatment arm, are fitted using the
specified formulas. Predictions are generated for each individual from each
treatment model, representing the predicted outcome under each treatment
assignment. These predictions are marginalized over the covariates by
averaging to produce marginal estimates of the means. Finally, the contrast
is computed and returned.

Variance estimates can be obtained using the nonparametric bootstrap.
Unadjusted estimates can be obtained by using intercept only models.
}
\examples{

ex_1 <- example_1
ex_1$.r_4 <- 1*(!is.na(ex_1$y_4))

standardization(
  data = ex_1,
  estimand = "difference",
  y0_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
  y1_formula = y_4 ~ x_1 + x_2 + x_3 + x_4,
  family = gaussian,
  treatment_column = "tx",
  outcome_indicator_column = ".r_4"
)

}
