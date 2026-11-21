# mcstatsim 0.9.7

Plot fixes. The look of the three plots changes (bug fixes); no numeric result changes and
all existing calls still work.

* `plotperformance()`: no automatic title any more; new `title`, `subtitle` and `caption`
  arguments (`NULL` draws none). New `labels` (named display names for columns, used in
  axes, legend and strips), `palette` (default blue and vermillion, then Okabe-Ito, instead
  of the ggplot2 hue colours) and `scales` arguments. Facet strips read `name = value`.
* `plotperformance()`: `measure` may name several measures, drawn as one row of panels per
  measure with free y scales, and `ref` may be a named vector
  (`ref = c(bias = 0, coverage = 0.95)`) or one value per measure.
* `plotperformance()`: the error-bar aesthetics moved from the global mapping to the point-range
  layer, so layers added with `+` no longer need `inherit.aes = FALSE`.
* `plotzip()` and `plotconvergence()`: panels follow the factor levels of the `by` columns
  (numeric order for numbers) instead of alphabetical order ("10", "200", "25"), strips read
  `name = value`, and the `by` columns are kept in the plot data, so `+ facet_wrap(~ method)`
  works. `condition` is now a factor in panel order.
* `plotzip()`: `true` may be a number as well as a column name; no automatic title (new
  `title`, `subtitle`, `caption`); new `labels`, `scales` and `colours` arguments; facets
  now share the x axis by default (`scales = "fixed"`, was `"free_x"`) so intervals can be
  compared across conditions; thicker legend keys; colours blue and vermillion.
* `plotconvergence()`: for a 0/1 column (running rejection rate or coverage) the band is now
  the Wilson score interval, which stays inside [0, 1], instead of the Wald band that went
  below 0 at small numbers of replications. New `band` argument (`"auto"`, `"wald"`,
  `"wilson"`), plus `labels`, `scales`, `title`, `subtitle` and `caption`; no automatic title.

# mcstatsim 0.9.6

* `runsim()`: `sim_func` may now return a named vector (`c(est = 1.2, se = 0.3)`)
  or a named list of equal-length vectors as well as a data frame; both are
  turned into a data frame.
* `checkestimates()` no longer fails with a `match.arg()` error when a `by`
  column is named `method` (or `decreasing`, `na.last`), as `summarisesim()`
  in 0.9.5.
* `runsim(backend = "psock")` now sends jobs to the workers in chunks (about
  four per worker) instead of one socket round trip per job. Runs with many
  small replications were up to 100 times slower than the fork backend, which
  made the vignette build look hung under knitr. Seeded results are unchanged.

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

