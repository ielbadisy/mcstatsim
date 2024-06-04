
# mcstatsim (under dev)

## Monte Carlo Simulation Tools Using a Functional Approach

The `mcstatsim` package is a lightweight, dependency-free tool designed
to facilitate statistical simulations through functional programming. It
centralizes the simulation process into a single higher-order function,
enhancing manageability and usability without adding overhead from
external dependencies. The package includes ready-to-use functions for
common simulation targets and processes simulation parameters via lists
or expanded grids, all while utilizing parallel processing to improve
efficiency on macOS and Linux-based distributions.

## Installation

You can install the latest development version of `mcstatsim` from
GitHub:

``` r
# install.packages("devtools")
devtools::install_github("ielbadisy/mcstatsim")
```

## Usage

Here is a basic example to get you started with `mcstatsim`:

``` r
library(mcstatsim)

# Define a simple simulation function
sim_function <- function(a, b) {
  Sys.sleep(0.2)  # Simulate a time-consuming process
  return(data.frame(result = a + b))
}

# Generate a grid of parameters
params <- expand.grid(a = 1:3, b = 4:6)

# Run simulations
results <- runsim(n = 5, grid_params = params, sim_func = sim_function)
```

This example demonstrates how to define a simple simulation function,
create a grid of parameters for the simulation, and run the simulations
in parallel using `mcstatsim`.

## Working example

``` r
set.seed(123)
```

## Features

  - **No Dependencies**: `mcstatsim` is designed to be lightweight and
    standalone, requiring no additional packages for its core
    functionalities, simplifying installation and usage.
  - **Functional programming approach**: Streamlines the process of
    setting up and running simulations.
  - **Parallel execution**: Leverages multiple cores to speed up the
    execution of simulations.
  - **Structured output**: Returns simulation results in a dataframe,
    facilitating analysis and visualization.

## Limitations

  - **Parallel processing mode**: `mcstatsim` utilizes parallel
    processing in multicore mode, which is only available on macOS and
    Linux-based distributions. Users on Windows may experience reduced
    performance or need to adjust for single-threaded execution.

## Contributing

Contributions to `mcstatsim` are welcome\! Please refer to the
[CONTRIBUTING.md](CONTRIBUTING.md) file for guidelines on how to make
contributions.

## License

`mcstatsim` is licensed under the AGPL-3. Please see the
[LICENSE](LICENSE.md) file for more details.

## Contact

For questions or feedback, please contact the package maintainer at
`elbadisyimad@gmail.com`.
