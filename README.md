
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `impart`: A package for designing, monitoring, and analyzing randomized trials

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/jbetz-jhu/impart/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jbetz-jhu/impart/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/jbetz-jhu/impart/graph/badge.svg?token=75LYU8AN1H)](https://codecov.io/gh/jbetz-jhu/impart)  
<!-- badges: end -->

Investigators are faced with many challenges in designing efficient,
ethical randomized trials due to competing demands: a trial must collect
enough information to identify meaningful benefits or harms with a
desired probability while also minimizing potential harm and suboptimal
treatment of participants. Satisfying these competing demands is further
complicated by the limited and imprecise information available during
the design of a study.

Studies designed around a fixed sample size are inflexible, requiring
investigators to wait until the end of data collection to perform
statistical analyses. Group sequential designs are more flexible,
allowing studies to be stopped for efficacy or futility according to a
pre-planned analyses, which occur when the number of obtained primary
outcomes reach pre-specified fractions of the final sample size.

Covariate adjustment allows investigators to potentially gain additional
precision by utilizing information collected from individuals prior to
randomization in the statistical analysis. This potential increase in
precision can be used to provide additional power in a fixed sample size
design or a group sequential design. Not all methods of covariate
adjustment are directly compatible with group sequential designs, but a
broad class of methods can be made compatible by performing an
orthogonalization of the resulting estimates and their
variance-covariance matrix (Van Lancker, Betz, and Rosenblum 2022). This
package enables implementing this orthogonalization.

Another disadvantage of covariate adjustment is that the amount of
precision gained from covariate adjustment is not known precisely at the
outset of a study. This complicates the ability to use covariate
adjustment to reduce the required sample size instead of providing
additional power. Rather than planning analyses based on a specific
number of participants, investigators can pre-specify when analyses
reach pre-specified levels of precision: this is known as information
monitoring (Mehta and Tsiatis 2001). This allows investigators to adapt
their study to the precision in the accruing data, reducing the risk of
under- or overpowered trials. This also allows investigators to use
covariate adjustment to shorten the trial duration, rather than just
providing additional power and precision.

The `impart` package can be used for performing covariate adjustment in
group sequential designs or designing, monitoring, and analyzing
information monitored designs.

## Installation

You can install the development version of impart from
[GitHub](https://github.com/) using `install_github` from the `devtools`
package:

``` r
# install.packages("devtools") # If not already installed, install devtools
devtools::install_github("jbetz-jhu/impart")
```

There are several vignettes built into `impart`: These are listed in the
‘Articles’ tab above, and can be listed in the R console:

``` r
vignette(package = "impart")
```

| Title | Item |
|:---|:---|
| Covariate Adjustment in Group Sequential Designs (source, html) | analyses_group_sequential |
| Designing Information Monitored Trials for Binary Outcomes (source, html) | design_binary |
| Designing Information Monitored Trials for Continuous Outcomes (source, html) | design_continuous |
| Designing Information Monitored Trials for Time-to-Event Outcomes (source, html) | design_time_to_event |
| Getting Started with `impart` (source, html) | impart |
| Implementing New Methods in `impart` (source, html) | new_methods_in_impart |
| Monitored Analyses for a Binary Outcome (source, html) | analyses_binary |
| Monitored Analyses for a Continuous Outcome (source, html) | analyses_continuous |
| Monitored Analyses for a Time-to-Event Outcome (source, html) | analyses_time_to_event |
| Monitoring Information for a Binary Outcome (source, html) | monitoring_binary |
| Monitoring Information for a Continuous Outcome (source, html) | monitoring_continuous |
| Monitoring Information for a Time-to-Event Outcome (source, html) | monitoring_time_to_event |

**NOTE:** `impart` is tested using the [`testthat`
package](https://testthat.r-lib.org/) with a continuous integration
workflow, and test coverage assessed using
[codecov](https://codecov.io/gh/jbetz-jhu/impart). Vignettes currently
cover the complete workflow for trials with a continuous outcome. Other
vignettes on binary, ordinal, and time-to-event outcomes are under
active development. Please check back to see if there have been updates
to the `impart` software or documentation.

------------------------------------------------------------------------

## Background for Group Sequential Designs Under Violation of Independent Increments Property

Group sequential designs (GSD) are a commonly used type of clinical
trial design that involves pre-planned interim analyses where the trial
can be stopped early for efficacy or futility. These designs are
prevalent in confirmatory clinical trials for ethical and efficiency
reasons as they potentially save time and resources by allowing early
termination of the trial.

------------------------------------------------------------------------

### Incompatibility

Many covariate adjusted estimators are incompatible withcommonly used
stopping boundaries in GSDs, when models used to construct the
estimators are misspecified. Specifically, to apply GSDs, the sequential
test statistics need to have the independent increments covariance
structure in order to control Type I error Jennison and Turnbull (1997).
However, this general theory of Scharfstein, Tsiatis, and Robins (1997)
and Jennison and Turnbull (1997) is not guaranteed to hold for covariate
adjusted estimators under model misspecification, which is likely to be
the case in practice. In particular, under model misspecification,
covariate adjusted estimators can fail to have this independent
increments property when using data (e.g., baseline covariates,
short-term endpoints) of pipeline patients (i.e., patients enrolled but
not in the study long enough to have their primary outcomes measured at
the (interim) analysis). This lack of independent increments can
generally occur when estimators use working models; see e.g., Rosenblum
et al. (2015) for augmented inverse probability weighted estimators and
Shoben and Emerson (2014) for estimators based on generalized estimating
equations. A long list of further examples is provided by Jennison and
Turnbull (1997) and Kim and Tsiatis (2020).

------------------------------------------------------------------------

### Solution: Orthogonalization

We implement the general method of Van Lancker, Betz, and Rosenblum
(2022) that extends the highly useful and fundamental theory of
information-monitoring in GSDs Jennison and Turnbull (1997) so that it
can be used with any regular, asymptotically linear estimator. This
covers many estimators in RCTs. The method uses orthogonalization to
produce modified estimators that (1) have the independent increments
property needed to apply GSDs, and (2) simultaneously improve (or leave
unchanged) the variance at each analysis.

------------------------------------------------------------------------

## Background for Information Monitoring

We can estimate the precision required to achieve power $(1 - \beta)$ to
identify a treatment effect $\delta$ with a $s$-sided test with type I
error rate $\alpha$ using:

$$\mathcal{I} = \left(\frac{Z_{\alpha/s} + Z_{\beta}}{\delta}\right)^2 \approx \frac{1}{\left(SE(\hat{\delta})\right)^2} = \frac{1}{Var(\hat{\delta})}$$

This uses the square of the empirical standard error (or the empirical
variance estimate) to measure the precision to which the treatment
effect $\delta$ can be measured with the data in hand. A
precision-adaptive design can reduce the risk of under- or overpowered
trials by collecting data until the precision is sufficient to conduct
analyses.

------------------------------------------------------------------------

### Approximate Precision vs. Sample Size

Let $T$ denote treatment and $C$ denote control, and $Y^{(A)}$ denote
the outcome of interest under treatment assigment $A$, where $A = 1$
indicates assignment to the treatment arm and $A = 0$ denotes assignment
to the control arm.

For a continuous outcome, the required information to estimate the
difference in means $\delta_{DIM} = E[Y^{(1)}] - E[Y^{(0)}]$ depends on
the sample size and variances of outcomes in each treatment arm:

$$SE(\delta) = \sqrt{\frac{\sigma^{2}_{T}}{n_{T}} + \frac{\sigma^{2}_{C}}{n_{C}}}$$

For a binary outcome, the required information to estimate the risk
difference $\delta_{RD} = E[Y^{(1)}] - E[Y^{(0)}]$ depends on the
response rate in the control arm $(\pi_{C} = \pi_{T} - \delta)$:

$$SE(\delta) = \sqrt{\frac{\pi_{T}(1 - \pi_{T})}{n_{T}} + \frac{\pi_{C}(1 - \pi_{C})}{n_{C}}}$$

For an ordinal outcome with $K$ categories, let
$\pi_{A}^{K} = Pr\{Y^{(A)} = k\}$ denote the probability of an outcome
in category $k$ under treatment $A$. The Mann-Whitney estimand $\phi$ is
the probability of having an outcome as good or better under the
treatment arm relative to control with an adjustment for ties:

$$\phi = Pr\{Y^{(T)} > Y^{(C)}\} + \frac{1}{2} Pr\{Y^{(T)} = Y^{(C)}\}$$

This is also known as the competing probability. The
precision/information depends on $\phi$ (Fay and Malinovsky 2018):

$$SE(\delta) \approx \sqrt{\frac{\phi(1 - \phi)}{n_{T}n_{C}}\left(1 + \left(\frac{n_{T} + n_{C} - 2}{2}\right)\left(\frac{\phi}{1 + \phi} + \frac{1 - \phi}{2 - \phi} \right)\right)}$$

Alternatively, the precision/information can be obtained from the
distribution of outcomes under each treatment arm (Zhao, Rahardja, and
Qu 2008). Let $N = n_{T} + n_{C}$:

$$SE(\delta) = \sqrt{\frac{1}{12(n_{T}n_{C})}\left(N+1 - \frac{1}{N(N-1)}\right)\sum_{k = 1}^{K}(\pi_{T}^{k}n_{T} + \pi_{C}^{k}n_{C})}$$

Expressions for the information for other estimands can be obtained
elsewhere (Jennison and Turnbull 1999). In practice, the parameters in
these expressions are not precisely known a priori. The advantage of an
information monitoring design is that the sample size is not fixed a
priori based on estimates of these parameters, but adapts automatically
to the precision of the accruing data.

------------------------------------------------------------------------

### Covariate Adjustment in Randomized Trials

Covariate adjusted analyses can also give greater precision than an
unadjusted analyses without introducing more stringent assumptions,
however the amount of precision gained in adjusted analyses are also not
precisely known a priori (Benkeser et al. 2020). Instead of predicating
the design on assumptions about the potential gain in precision from
covariate adjustment, a precision-adaptive design automatically adjusts
the sample size accordingly.

The relative efficiency of a covariate adjusted estimator to an
unadjusted estimator is $RE_{A/U} = Var(\theta_{U})/Var(\theta_{A})$.
The relative change in variance of a covariate-adjusted analysis to an
unadjusted analysis is:

$$RCV_{A/U} = \frac{Var(\theta_{A}) - Var(\theta_{U})}{Var(\theta_{U})} = \frac{1}{RE_{A/U}} - 1$$
Alternatively, $RE_{A/U} = 1/(1 + RCV_{A/U})$: If a covariate adjusted
analysis has a relative efficiency of 1.25, the relative change in
variance would be -0.2, or a -20% change in variance. Since precision is
the inverse of variance, the relative change in precision of a
covariate-adjusted analysis to an unadjusted analysis is:

$$RCP_{A/U} = \frac{1/Var(\theta_{A}) - 1/Var(\theta_{U})}{1/Var(\theta_{U})} = Var(\theta_{U})/Var(\theta_{A}) - 1 = RE_{A/U} - 1$$
Alternatively, $RE_{A/U} = 1 + RCP_{A/U}$: If a covariate adjusted
analysis has a relative efficiency of 1.25, the relative change in
precision would be 0.25, or a 25% change in precision.

------------------------------------------------------------------------

### Sequential Analyses

Pre-planned interim analyses allow investigators to stop a randomized
trial early for efficacy or futility (Jennison and Turnbull 1999).
Precision-adaptive trials can integrate both interim analyses and
covariate adjustment, using a broad class of methods (Van Lancker, Betz,
and Rosenblum 2022). Mehta and Tsiatis (2001) illustrate
information-adaptive designs in practice. For a tutorial on implementing
interim analyses, see Lakens, Pahlke, and Wassmer (2021).

------------------------------------------------------------------------

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Benkeser2020" class="csl-entry">

Benkeser, David, Iván Dı́az, Alex Luedtke, Jodi Segal, Daniel
Scharfstein, and Michael Rosenblum. 2020. “Improving Precision and Power
in Randomized Trials for COVID-19 Treatments Using Covariate Adjustment,
for Binary, Ordinal, and Time-to-Event Outcomes.” *Biometrics* 77 (4):
1467–81. <https://doi.org/10.1111/biom.13377>.

</div>

<div id="ref-Fay2018" class="csl-entry">

Fay, Michael P., and Yaakov Malinovsky. 2018. “Confidence Intervals of
the Mann-Whitney Parameter That Are Compatible with the
Wilcoxon-Mann-Whitney Test.” *Statistics in Medicine* 37 (27):
3991–4006. <https://doi.org/10.1002/sim.7890>.

</div>

<div id="ref-jennison1997group" class="csl-entry">

Jennison, Christopher, and Bruce W Turnbull. 1997. “Group-Sequential
Analysis Incorporating Covariate Information.” *Journal of the American
Statistical Association* 92 (440): 1330–41.

</div>

<div id="ref-Jennison1999" class="csl-entry">

Jennison, Christopher, and Bruce W. Turnbull. 1999. *Group Sequential
Methods with Applications to Clinical Trials*. Chapman; Hall/CRC.
<https://doi.org/10.1201/9780367805326>.

</div>

<div id="ref-kim2020independent" class="csl-entry">

Kim, KyungMann, and Anastasios A Tsiatis. 2020. “Independent Increments
in Group Sequential Tests: A Review.” *SORT-Statistics and Operations
Research Transactions* 44 (2): 223–64.

</div>

<div id="ref-Lakens2021" class="csl-entry">

Lakens, Daniel, Friedrich Pahlke, and Gernot Wassmer. 2021. “Group
Sequential Designs: A Tutorial,” January.
<https://doi.org/10.31234/osf.io/x4azm>.

</div>

<div id="ref-Mehta2001" class="csl-entry">

Mehta, Cyrus R., and Anastasios A. Tsiatis. 2001. “Flexible Sample Size
Considerations Using Information-Based Interim Monitoring.” *Drug
Information Journal* 35 (4): 1095–1112.
<https://doi.org/10.1177/009286150103500407>.

</div>

<div id="ref-rosenblum2015" class="csl-entry">

Rosenblum, Michael, Tianchen Qian, Yu Du, and Huitong and Qiu. 2015.
“Adaptive Enrichment Designs for Randomized Trials with Delayed
Endpoints, Using Locally Efficient Estimators to Improve Precision.”
https://biostats.bepress.com/jhubiostat/paper275. Dept. Of Biostatistics
Working Papers.

</div>

<div id="ref-scharfstein1997semiparametric" class="csl-entry">

Scharfstein, Daniel O, Anastasios A Tsiatis, and James M Robins. 1997.
“Semiparametric Efficiency and Its Implication on the Design and
Analysis of Group-Sequential Studies.” *Journal of the American
Statistical Association* 92 (440): 1342–50.

</div>

<div id="ref-shoben2014violations" class="csl-entry">

Shoben, Abigail B, and Scott S Emerson. 2014. “Violations of the
Independent Increment Assumption When Using Generalized Estimating
Equation in Longitudinal Group Sequential Trials.” *Statistics in
Medicine* 33 (29): 5041–56.

</div>

<div id="ref-VanLancker2022" class="csl-entry">

Van Lancker, Kelly, Joshua Betz, and Michael Rosenblum. 2022. “Combining
Covariate Adjustment with Group Sequential, Information Adaptive Designs
to Improve Randomized Trial Efficiency.” *arXiv Preprint
arXiv:1409.0473*. <https://doi.org/10.48550/ARXIV.2201.12921>.

</div>

<div id="ref-Zhao2008" class="csl-entry">

Zhao, Yan D., Dewi Rahardja, and Yongming Qu. 2008. “Sample Size
Calculation for the Wilcoxonmannwhitney Test Adjusting for Ties.”
*Statistics in Medicine* 27 (3): 462–68.
<https://doi.org/10.1002/sim.2912>.

</div>

</div>
