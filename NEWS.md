# mcstatsim 0.9.1

* The result-producing functions gain a `digits` argument (default `4`;
  `NULL` to disable rounding): `calc_bias()`, `calc_variance()`, `calc_mse()`,
  `calc_rmse()`, `calc_coverage()`, `calc_width()`, `calc_rejection_rate()`,
  `calc_relative_bias()`, `calc_relative_mse()`, `calc_relative_rmse()`,
  `summarise_sim()` (rounds the measure columns only), `failure_summary()`
  (`failure_rate`) and `check_estimates()` (`prop_bad`).

# mcstatsim 0.9.0

## `runsim()` engine

* All `n * nrow(grid_params)` jobs are now dispatched in a single load-balanced
  parallel pass instead of looping over replications and resynchronising the
  worker pool after each one. The result layout is unchanged: one `ID` column
  holds the replication index.
* The condition's `grid_params` columns are prepended to the result (those that
  `sim_func` did not itself return), so the output identifies its own conditions
  and is ready for `summarise_sim()` or `aggregate()` without echoing parameters
  inside `sim_func`.
* New `seed` argument. When supplied, each job draws from its own independent
  `"L'Ecuyer-CMRG"` stream derived from `seed`, so a run is bit-for-bit
  reproducible and its result does not depend on `num_cores` or on the order in
  which jobs finish. The seed is stored in `attr(x, "seed")`. When `seed` is
  `NULL` (default) the session RNG is left untouched.
* New `on_error` argument (`"stop"`, the default and previous behaviour;
  `"warn"`; `"omit"`). With `"warn"` or `"omit"`, jobs in which `sim_func` throws
  are dropped and every failure is logged in `attr(x, "errors")` (condition,
  replication index, message). Warnings emitted by `sim_func`, which parallel
  backends would otherwise swallow, are collected in `attr(x, "warnings")`.
* New `save_path` and `checkpoint_every` arguments. With `save_path` set,
  replications run in chunks and the checkpoint file is rewritten after each
  chunk; calling `runsim()` again with the same `save_path` and matching
  `n` / `grid_params` / `seed` resumes, loading completed replications from disk
  and running only the rest. Per-job RNG streams are keyed by position, so a
  resumed run reproduces an uninterrupted one exactly.
* Stricter input validation (scalar positive-integer `n`, non-empty
  `grid_params`, and a clear error when `sim_func` returns a non-data-frame).

## Evaluation and diagnostics

* New `summarise_sim()` collapses replicate-level output into one row per
  condition, evaluating performance measures given as one-sided formulas such as
  `~ calc_bias(est, true_val[1])` or as functions of the group data frame. Each
  `calc_*` measure contributes its estimate and MCSE columns; colliding output
  names are an error.
* New `failure_summary()` aggregates `attr(x, "errors")` into one row per
  condition with the failure count, failure rate, and an example message.
* New `check_estimates()` scans the numeric columns of a result for
  `NA` / `NaN` / `Inf` and reports the count and proportion per condition.
* New `mcse_target()` returns how many replications are needed to reach a target
  Monte Carlo standard error, from the MCSE reached so far.
* New plotting helpers, which require the suggested `ggplot2`:
  `plot_convergence()` (running mean vs replication with an MCSE band),
  `plot_zip()` (the zip plot of Morris, White and Crowther (2019) for
  confidence-interval coverage) and `plot_performance()` (point-range of a
  `summarise_sim()` measure and its MCSE across the design).
* `ggplot2` added to `Suggests`.

## Fixes

* `calc_relative_rmse()` now returns a correct Monte Carlo standard error. The
  previous version reported `sqrt(rel_mse_mcse)`, which is not a valid
  transformation; the MCSE is now obtained from the MCSE of the relative MSE by
  the delta method, `rel_mse_mcse / (2 * rel_rmse)`, and is `NA` when
  `rel_rmse` is zero.

# mcstatsim 0.5.1

* Switched the parallel-map backend used by `mcpmap()` from `pbapply::pbmapply`
  to `functionals::fmapn()`, matching the equal-length-named-list mapping
  semantics with sequential/multicore/cluster execution and an optional
  progress bar. `runsim()` is unaffected.
* `pbapply` is no longer a dependency; `functionals` is now the sole `Imports`.
* Standardized `Authors@R` casing and split `URL`/`BugReports` so `URL` holds
  only the CRAN package page and `BugReports` only the GitHub issues page.
* Simplified the `Description:` field.

# mcstatsim 0.5.0

* Added a `NEWS.md` file to track changes to the package.

* Introduced parallel computing support via the `future` package implemented in the `pbapply` package.

* The unique dependency is now the `pbapply` package.

* This update addresses the limitation of the previous version, which only supported multicore parallel computing on Unix-based operating systems. The new version supports parallel computing across different operating systems, including Windows.

* Improved documentation and examples to reflect the new parallel computing capabilities.

