# mcstatsim 0.6.0

* `runsim()` now dispatches all `n * nrow(grid_params)` jobs in a single
  load-balanced parallel pass instead of looping over replications and
  resynchronising the worker pool after each one. The result layout is
  unchanged (one `ID` column holding the replication index).
* `runsim()` gains a `seed` argument. When supplied, each job draws from its
  own independent `"L'Ecuyer-CMRG"` stream derived from `seed`, so a run is
  bit-for-bit reproducible and its result does not depend on `num_cores` or
  on the order in which jobs finish. The seed is stored in `attr(x, "seed")`.
  When `seed` is `NULL` (default) the session RNG is left untouched.
* `runsim()` validates its inputs more strictly (scalar positive-integer `n`,
  non-empty `grid_params`, and a clear error when `sim_func` returns a
  non-data-frame for any job).
* `runsim()` gains an `on_error` argument (`"stop"`, the default and previous
  behaviour; `"warn"`; `"omit"`). With `"warn"` or `"omit"`, jobs in which
  `sim_func` throws are dropped and every failure is logged in
  `attr(x, "errors")` (condition, replication index, message).
* `runsim()` now captures warnings emitted by `sim_func` (which parallel
  backends would otherwise swallow) into `attr(x, "warnings")`.
* New `failure_summary()` aggregates `attr(x, "errors")` into one row per
  condition with the failure count, failure rate, and an example message.

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

