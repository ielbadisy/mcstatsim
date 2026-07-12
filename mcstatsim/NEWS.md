# mcstatsim 0.5.2

* Reworked the simulation backend to use a chunk-aware base `parallel` backend with faster task dispatch on Unix-like systems.

* Added a master-side text progress bar that remains visible during parallel execution.

* Added `data.table`-backed result binding for lower overhead and better throughput.

* Added a benchmark section comparing the backend against `pbapply` and `future.apply` on a heavy synthetic workload using `bench::mark()`.

* Updated package metadata for the next release.
