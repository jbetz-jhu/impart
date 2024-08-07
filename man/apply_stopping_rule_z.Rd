% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_stopping_rule_z.R
\name{apply_stopping_rule_z}
\alias{apply_stopping_rule_z}
\title{Apply group sequential stopping rules to test statistics}
\usage{
apply_stopping_rule_z(
  test_statistics,
  trial_design,
  information_fraction,
  information_target
)
}
\arguments{
\item{test_statistics}{A numeric vector of standardized test statistics}

\item{trial_design}{A \code{TrialDesignGroupSequential object} created by
\code{\link[rpact:getDesignGroupSequential]{rpact::getDesignGroupSequential()}}}

\item{information_fraction}{A numeric vector containing the observed
information fractions}

\item{information_target}{A numeric scalar containing the target information
level, created by \code{\link[=required_information_single_stage]{required_information_single_stage()}}}
}
\value{
A list containing an updated \code{TrialDesignGroupSequential object}
object, the resulting decision, and supporting data.
}
\description{
Group sequential designs allow investigators to perform pre-planned interim
assessments of futility or efficacy in an ongoing trial while preserving the
Familywise Type I Error Rate (FWER). Differences between the observed
information levels and those specified in the initial design require the
boundaries to be updated in order to preserve FWER control. This function
allows users to update the boundaries according to observed information
levels, apply stopping rules, and return their resulting decisions.
}
\examples{

# Two-sided design with O'Brien-Fleming efficacy stopping and non-binding
# O'Brien Fleming Beta Spending Futility with 2 interim analyses at 50\% and
# 75\%

# Create Initial `TrialDesignGroupSequential` Object:
two_sided_of_efficacy_nb_of_futility_0 <-
 rpact::getDesignGroupSequential(
   alpha = 0.05, # 5\% Type I Error
   beta = 1 - 0.80, # 80\% Power
   sided = 2, # Two-Sided Test
   # Interim analyses at 50\%, 75\%, 100\% of Information Target
   informationRates = c(0.5, 0.75, 1),
   # Efficacy and Non-Binding Futility Stopping using O'Brien-Fleming
   # alpha- and beta- spending functions
   typeOfDesign = "asOF",
   typeBetaSpending = "bsOF",
   bindingFutility = FALSE
 )

 # Test Statistics at Interim Analysis 1
 test_statistics_1 <- c(1.0)

 # Observed information fraction at 1st analysis = 52.5\% vs. 50\% in design
 information_fraction_1 <- c(0.525)
 information_target <- 50

 analysis_decision_1 <-
   apply_stopping_rule_z(
     test_statistics = test_statistics_1,
     trial_design = two_sided_of_efficacy_nb_of_futility_0,
     information_fraction = information_fraction_1,
     information_target = information_target
   )

 # Test Statistics at Interim Analysis 2
 test_statistics_2 <- c(1.0, 2.2)

 # Observed information fraction at 2nd analysis = 74.5\% vs. 75\% in design
 information_fraction_2 <- c(0.525, 0.745)

 analysis_decision_2 <-
   apply_stopping_rule_z(
     test_statistics = test_statistics_2,
     trial_design =
     # Use updated design from analysis 1
     analysis_decision_1$trial_design_updated,
     information_fraction = information_fraction_2,
     information_target = information_target
 )

 # Test Statistics at Interim Analysis 3
 test_statistics_3 <- c(1.0, 2.2, 2.3)

 # Observed information fraction at 3rd analysis = 102\% vs. 100\% in design
 information_fraction_3 <- c(0.525, 0.745, 1.02)
 analysis_decision_3 <-
   apply_stopping_rule_z(
     test_statistics = test_statistics_3,
     trial_design =
     # Use updated design from analysis 1
     analysis_decision_2$trial_design_updated,
     information_fraction = information_fraction_3,
     information_target = information_target
 )
}
\seealso{
\code{\link[=required_information_single_stage]{required_information_single_stage()}} and
\code{\link[=required_information_mw_single_stage]{required_information_mw_single_stage()}} for obtaining the target
information level for a single stage design;
\code{\link[rpact:getDesignGroupSequential]{rpact::getDesignGroupSequential()}} for obtaining \code{trial_design}, and
\code{\link[=required_information_sequential]{required_information_sequential()}} for adjusting the information
level to multi-stage designs.
}
