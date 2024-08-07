
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `impart`: A package for designing, monitoring, and analyzing randomized trials

<!-- badges: start -->

[![R-CMD-check](https://github.com/jbetz-jhu/impart/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jbetz-jhu/impart/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/jbetz-jhu/impart/graph/badge.svg?token=75LYU8AN1H)](https://codecov.io/gh/jbetz-jhu/impart)  
<!-- badges: end -->

Investigators are faced with many challenges in designing efficient,
ethical randomized trials due to competing demands: a trial must collect
enough information to identify meaningful benefits or harms with a
desired probability, while also minimizing potential harm and suboptimal
treatment of participants. Satisfying these competing demands is further
complicated by the limited and imprecise information available during
the design of a study.

Studies designed around a fixed sample size are inflexible, requiring
investigators to wait until the end of data collection to perform
statistical analyses. Group sequential designs are more flexible,
allowing studies to be stopped for efficacy or futility according to a
pre-planned analyses. Covariate adjustment allows investigators to
potentially gain additional precision and power from variables collected
on individuals prior to randomization. Unfortunately, the amount of
precision gained from covariate adjustment is not known precisely at the
outset of a study, complicating the ability to use covariate adjustment
to reduce the required sample size. Additionally, some methods of
covariate adjustment do not provide the independent increments property
required by group sequential design methods.

Rather than planning analyses based on a specific number of
participants, investigators can pre-specify when analyses reach
pre-specified levels of precision: this is known as information
monitoring (Mehta and Tsiatis 2001). This allows investigators to adapt
their study to the precision in the accruing data, reducing the risk of
under- or overpowered trials. This also allows investigators to use
covariate adjustment to shorten the trial duration, rather than just
providing additional power and precision.

The independent increments property can also be obtained by
orthogonalizing estimates and their covariance, allowing covariate
adjustment to be included in both group sequential and information
monitoring designs.

## Installation

You can install the development version of impart from
[GitHub](https://github.com/) using:

``` r
# install.packages("devtools")
devtools::install_github("jbetz-jhu/impart")
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>       ✔  checking for file 'C:\Users\jbetz\AppData\Local\Temp\RtmpMxHB2F\remotes532460593196\jbetz-jhu-impart-f8934f5/DESCRIPTION'
#>       ─  preparing 'impart':
#>      checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>       ─  building 'impart_0.1.0.tar.gz'
#>      
#> 
```

## Vignettes

There are three vignettes on how to use `impart`, each focusing on a
different phase of a study. Each vignette is sequential, building upon
the previous ones.

``` r
vignette("impart_study_design", package = "impart") # Design
vignette("impart_monitoring", package = "impart") # Information Monitoring
vignette("impart_analyses", package = "impart") # Analyses
```

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

<div id="ref-Jennison1999" class="csl-entry">

Jennison, Christopher, and Bruce W. Turnbull. 1999. *Group Sequential
Methods with Applications to Clinical Trials*. Chapman; Hall/CRC.
<https://doi.org/10.1201/9780367805326>.

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
