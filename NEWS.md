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

