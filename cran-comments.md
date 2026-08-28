## Submission

This is a feature update from the CRAN version 0.5.1 to 0.9.0. It adds
reproducible per-replication RNG streams, error and warning capture, checkpoint
and resume for long runs, a `summarise_sim()` evaluation layer, and diagnostic
helpers (`failure_summary()`, `check_estimates()`, `mcse_target()`) and plots.
See NEWS.md for the full list. `calc_relative_rmse()` has a corrected Monte
Carlo standard error.

`ggplot2` is added to Suggests for the three plotting helpers; it is used
conditionally and the rest of the package has no new dependencies (Imports is
still `functionals` only).

## Test environments

* Local Ubuntu 24.04, R 4.5.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
