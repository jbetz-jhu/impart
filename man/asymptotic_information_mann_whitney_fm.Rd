% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/asymptotic_information_mw.R
\name{asymptotic_information_mann_whitney_fm}
\alias{asymptotic_information_mann_whitney_fm}
\title{Approximate information from a random samples of a given size for ordinal
outcomes}
\usage{
asymptotic_information_mann_whitney_fm(
  n_0,
  n_1,
  mw = NULL,
  pmf_1 = NULL,
  pmf_0 = NULL,
  adjust = TRUE
)
}
\arguments{
\item{n_0}{Numeric vector containing the sample size in the control arm}

\item{n_1}{Numeric vector containing the sample size in the treatment arm}

\item{mw}{Numeric vector containing the Mann-Whitney estimand}

\item{pmf_1}{Numeric vector or matrix of row vectors, each containing the
probability mass function of outcomes in the population of individuals
receiving the active intervention}

\item{pmf_0}{Numeric vector or matrix of row vectors, each containing the
probability mass function of outcomes in the population of individuals
receiving the control intervention}

\item{adjust}{(Scalar: Logical) Should the estimand be adjusted for ties?
NOTE: This can only be computed when \code{pmf_0} and \code{pmf_1} are
supplied.}
}
\value{
When all parameters are scalars, the result is a scalar, indicating
the approximate information. When multiple values are specified, a grid of
unique parameters are constructed, and the approximate information is
computed for each value of the parameters.
}
\description{
These functions provide an asymptotic approximation to the information
(i.e. precision, inverse of the variance) provided by two samples under
either an assumed value of the Mann-Whitney estimand or probability mass
functions (PMFs) of an ordinal outcome in treatment and control populations.
}
\examples{
# When a single value is supplied for each parameter, a scalar is returned:
asymptotic_information_mann_whitney_fm(
    n_0 = 100,
    n_1 = 100,
    mw = 0.75,
    adjust = FALSE
  )

# When multiple values are supplied for one or more parameters, the grid of
# parameters are created, and a data.frame is returned.
asymptotic_information_mann_whitney_fm(
  n_0 = c(100, 150),
  n_1 = c(100, 150),
  mw = 0.75,
  adjust = FALSE
)


# Specifying PMFs - With and Without Tie Adjustment
asymptotic_information_mann_whitney_fm(
  n_0 = 100,
  n_1 = 100,
  pmf_0 = c(0.2, 0.2, 0.6),
  pmf_1 = c(0.1, 0.1, 0.8),
  adjust = TRUE
)

# Specifying Multiple PMFs
asymptotic_information_mann_whitney_fm(
  n_0 = 100,
  n_1 = 100,
  pmf_0 =
    rbind(
      c(0.2, 0.2, 0.6),
      c(0.3, 0.1, 0.6)
    ),
  pmf_1 =
    rbind(
      c(0.1, 0.1, 0.8),
      c(0.05, 0.05, 0.9)
      ),
  adjust = TRUE
)
}
\seealso{
\link{asymptotic_information_difference_means} and
\link{asymptotic_information_difference_proportions} for continuous and
binary outcomes, respectively.
}
