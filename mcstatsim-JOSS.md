---
title: 'mcstatsim: Functional Monte Carlo simulation in R with process-based parallelism'
tags:
  - R
  - monte carlo
  - simulation
  - parallel computing
  - statistics
authors:
  - name: Imad EL BADISY
    affiliation: "1"
    email: elbadisyimad@gmail.com
affiliations:
  - name: Independent researcher
    index: 1
citation_author: EL BADISY
date: 12 July 2026
year: 2026
bibliography: paper.bib
output: rticles::joss_article
journal: JOSS
---



# Summary

`mcstatsim` is an R package for Monte Carlo simulation studies that keeps the
user-facing interface compact while supporting arbitrary user-supplied
simulation functions. The core workflow takes a parameter grid, expands it
across replications, and evaluates each task in parallel before binding the
results into a tabular output that is straightforward to analyze downstream.

The package is designed for heavy simulation workloads where process-based
parallelism has enough work to amortize cluster overhead. That design choice is
important because many simulation studies need to call ordinary R code rather
than compiled code, so the backend must remain compatible with arbitrary
functions supplied by the user.

# Statement of need

Simulation studies in statistics often involve repeated execution of an R
function over a parameter grid, followed by computation of estimators, bias,
coverage, or error metrics. In practice, the main engineering burden is not the
statistical calculation itself, but the orchestration of many tasks, the
collection of outputs, and the safe use of parallel workers.

`mcstatsim` addresses that problem by providing a functional interface centered
on `runsim()` and `mcpmap()`. The package accepts arbitrary R simulation
functions, preserves a visible progress bar during execution, and returns
data-frame outputs that are easy to summarize with standard R tools.

# Implementation

The current backend uses base `parallel` with a task-oriented execution model.
Tasks are prepared once, dispatched in chunks, and combined with
`data.table::rbindlist()` [@data.table] to reduce binding overhead. On
Unix-like systems, the package uses a forked fast path when progress reporting
is disabled; otherwise it falls back to a PSOCK cluster so the same code works
across platforms.

This architecture keeps the implementation in R, which is the right trade-off
for a package whose primary objective is to safely parallelize arbitrary
user-supplied simulation functions. It also keeps the progress bar on the
master process, so progress reporting remains usable while workers are busy.

# Benchmark results

We compared `mcstatsim` against two commonly used alternatives for parallel
apply-style workflows: `pbapply` [@pbapply] and `future.apply` [@future]. The
benchmark used a heavy synthetic Monte Carlo workload with 60 replications over
a 16-cell parameter grid, evaluated on four workers. Timing was measured with
`bench::mark()` [@bench] using memory profiling disabled for the parallel cases.

The benchmark results are shown in Table 1 and Figure 1. On this workload,
`mcstatsim` had the shortest median runtime.




Table: Benchmark comparison on the heavy synthetic workload used during development.

|Backend      | Median runtime (s)|Runtime vs. `mcstatsim` |Gain vs. `mcstatsim` |
|:------------|------------------:|:-----------------------|:--------------------|
|mcstatsim    |              0.810|1.00x                   |0%                   |
|pbapply      |              0.922|1.14x                   |12.1%                |
|future.apply |              1.250|1.54x                   |35.2%                |

![Median benchmark runtime for the compared backends on the synthetic Monte Carlo workload.](mcstatsim-JOSS_files/figure-latex/benchmark-plot-1.pdf) 

On this benchmark, `mcstatsim` reduced median runtime by 12.1% relative to
`pbapply` and by 35.2% relative to `future.apply`. These results are workload
dependent, but they show that a task-oriented backend with chunked process-based
parallelism can provide a measurable advantage for heavy Monte Carlo studies.

# Limitations

`mcstatsim` is not intended to accelerate very small jobs. As with other
process-based parallel approaches in R, cluster startup and serialization costs
can dominate when each simulation task is too small. The package is therefore
best suited to simulation studies with enough computation per task to justify
parallel dispatch.

# Acknowledgements

This work uses R [@R] and several community-developed packages. The benchmark
comparison was informed by `pbapply` [@pbapply], `future.apply` [@future],
`data.table` [@data.table], and `bench` [@bench].

# References
