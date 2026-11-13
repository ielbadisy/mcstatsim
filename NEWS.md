# mcstatsim 0.9.5

* `summarisesim()` no longer fails with a `match.arg()` error when a `by`
  column is named `method` (or `decreasing`, `na.last`): the grouping columns
  were passed by name to `order()`.
* Rounding with `digits` (in `summarisesim()` and the `calc*()` functions) now
  keeps `digits` significant digits for values smaller than 1 in absolute
  value, so tiny quantities such as a width of 2e-5 are no longer rounded to 0
  or 1e-4. Values of 1 or more are still rounded to `digits` decimal places.
* `runsim(backend = "auto")` now uses a PSOCK cluster on Linux when the session
  already runs an OpenMP or threaded BLAS thread pool (libgomp, MKL, OpenBLAS,
  TBB), typically started by matrix algebra earlier in a long script. Forked
  workers inherited that pool and hung with idle workers.
* `runsim()` reports a worker process that crashes or is killed as a failed job
  ("worker process died or did not return a result"), so it surfaces through
  `on_error` instead of a confusing error or a lost result.
* The deprecated underscore aliases (`calc_coverage()`, ...) are still exported
  and still call `.Deprecated()`; documentation and vignettes use only the new
  names.

# mcstatsim 0.9.4

Exported functions are now glued lowercase, with no underscores. The old names
still work but are deprecated and will be removed in a future release.

| Old name | New name |
|----------|----------|
| `calc_bias()` | `calcbias()` |
| `calc_coverage()` | `calccoverage()` |
| `calc_mse()` | `calcmse()` |
| `calc_rejection_rate()` | `calcrejectionrate()` |
| `calc_relative_bias()` | `calcrelativebias()` |
| `calc_relative_mse()` | `calcrelativemse()` |
| `calc_relative_rmse()` | `calcrelativermse()` |
| `calc_rmse()` | `calcrmse()` |
| `calc_variance()` | `calcvariance()` |
| `calc_width()` | `calcwidth()` |
| `check_estimates()` | `checkestimates()` |
| `combine_df()` | `combinedf()` |
| `failure_summary()` | `failuresummary()` |
| `mcse_target()` | `mcsetarget()` |
| `plot_convergence()` | `plotconvergence()` |
| `plot_performance()` | `plotperformance()` |
| `plot_zip()` | `plotzip()` |
| `summarise_sim()` | `summarisesim()` |

