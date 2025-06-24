# impart 0.2.0

This release includes expanded functionality and bug fixes for time-to-event outcomes.

## New Features

* A vignette for time-to-event outcomes has been created, illustrating the analysis of a covariate-adjusted marginal hazard ratio using `speff2trial::speffsurv()`.
* A doubly-robust estimator for the average treatment effect (difference in means, risk difference, relative risk) is now available: `dr_joffe()`

## Bug Fixes:

* Time-to-event outcomes should work with monitoring functinos, such as `prepare_monitored_study_data()`, `count_outcomes()`
* `initialize_monitored_design()` should work appropriately when `rpact::getDesignGroupSequential` involves only a single stage.

# impart 0.1.0

* Initial release: This contains functions for planning, monitoring, and analyzing trials with continuous and binary outcomes.
