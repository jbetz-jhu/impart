% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information_trajectory.R
\name{information_trajectory}
\alias{information_trajectory}
\title{Reconstruct a trajectory of information accrual}
\usage{
information_trajectory(
  prepared_data,
  monitored_design = NULL,
  estimation_function,
  estimation_arguments,
  correction_function = NULL,
  orthogonalize,
  bootstrap = TRUE,
  n_min = 30,
  n_increment = 5,
  rng_seed,
  control = monitored_analysis_control()
)
}
\arguments{
\item{prepared_data}{A prepeared dataset: see
\link{prepare_monitored_study_data}}

\item{monitored_design}{An object of class \code{monitored_design} created
using \code{\link[=initialize_monitored_design]{initialize_monitored_design()}}}

\item{estimation_function}{A function whose arguments include a data.frame
named \code{data}}

\item{estimation_arguments}{A list of any additional arguments needed by
\code{estimation_function}}

\item{correction_function}{An optional function to adjust the variance
estimate using parameters from \code{estimation_arguments}}

\item{orthogonalize}{A \code{logical} scalar:: Should estimates, their
covariance, and the resulting test statistics be orthogonalized?}

\item{bootstrap}{A \code{logical} scalar: should bootstrap be used?}

\item{n_min}{A \code{numeric} scalar indicating the minimum sample size for
the information trajectory}

\item{n_increment}{A \code{numeric} scalar indicating the increment in sample
size for calculating the trajectory from \code{n_min} to the current sample size
in \code{prepared_data}.}

\item{rng_seed}{Numeric scalar containing the L'Ecuyer pseudorandom
number generator seed}

\item{control}{A list containing the control arguments for computation,
typically created with \code{\link[=monitored_analysis_control]{monitored_analysis_control()}}}
}
\value{
A \code{data.frame} containing the information and number of outcome
events at each analysis
}
\description{
While \code{\link[=estimate_information]{estimate_information()}} provides an estimated information
level at a single point, investigators may want to visualize a trajectory
of how quickly information is accruing as participants are randomized and
their outcomes are obtained. \code{information_trajectory} attempts to
reconstruct the study data available at different points in time, and then
compute the information accrued at these times. Regression models can be used
to smooth these trajectories to provide projections of when the information
will reach pre-specified thresholds.
}
\examples{
# To be added
}
