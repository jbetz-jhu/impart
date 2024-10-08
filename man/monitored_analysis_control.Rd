% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitored_analysis_control.R
\name{monitored_analysis_control}
\alias{monitored_analysis_control}
\alias{monitored_analysis_control_testing}
\title{Control arguments for performing information monitored analyses}
\usage{
monitored_analysis_control(
  orthogonal_and_oblique = FALSE,
  required_packages = character(0),
  n_bootstrap = 1000,
  n_cores = 1,
  use_load_balancing = FALSE
)

monitored_analysis_control_testing(
  orthogonal_and_oblique = FALSE,
  required_packages = character(0),
  n_bootstrap = 250,
  n_cores = 1,
  use_load_balancing = FALSE
)
}
\arguments{
\item{orthogonal_and_oblique}{A logical scalar: if \code{orthogonal == TRUE}
should non-orthogonalized (oblique) test statistics and decisions be
returned?}

\item{required_packages}{A character vector of required packages to be
loaded on a cluster: only used when \code{n_cores} > 1.}

\item{n_bootstrap}{Scalar number containing the number of bootstrap
replicates to perform}

\item{n_cores}{Scalar number of cores to use.}

\item{use_load_balancing}{Logical scalar: Should load balancing be used?}
}
\value{
A list containing the default parameters or supplied alternatives.
}
\description{
This provides a way to set and modify the analysis parameters for information
monitoring.
}
\examples{
# To be added
}
\seealso{
\code{\link[parallel:clusterApply]{parallel::parSapplyLB()}} and \code{\link[parallel:clusterApply]{parallel::parSapply()}} for parallel
computing; \link{monitored_analysis} for conducting information monitored
analyses, which uses \link{estimate_information} and
\code{\link[=calculate_covariance]{calculate_covariance()}} for computing the covariance of estimates.
}
