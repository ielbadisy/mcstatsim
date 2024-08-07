% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runsim.R
\name{runsim}
\alias{runsim}
\title{Run Monte Carlo Simulations in Parallel}
\usage{
runsim(
  n,
  grid_params,
  sim_func,
  show_progress = TRUE,
  num_cores = parallel::detectCores() - 1
)
}
\arguments{
\item{n}{The number of times the simulation function should be executed for each set of parameters. Must be a positive integer.}

\item{grid_params}{A dataframe where each row corresponds to a unique combination of parameters for the simulation. Typically generated using `expand.grid`.}

\item{sim_func}{The simulation function to be applied. This function should accept parameters corresponding to a row in `grid_params` and return a dataframe or a list that can be row-bound.}

\item{show_progress}{Logical indicating whether to display progress messages during the execution of the simulations.}

\item{num_cores}{The number of cores to use for parallel execution. The default is one less than the total number of cores available on the system.}
}
\value{
A combined dataframe of all simulation results.
}
\description{
This function executes a series of Monte Carlo simulations in parallel, providing detailed progress updates.
}
\details{
The function first validates the input parameters. It then uses parallel processing to apply `sim_func` to each combination of parameters specified in `grid_params`, repeating each simulation `n` times.
The results are combined into a single dataframe.
}
\examples{
\dontrun{
library(mcstatsim)

# Define a simple simulation function
sim_function <- function(a, b) {
  Sys.sleep(0.1)  # Simulate a time-consuming process
  return(data.frame(result = a + b))
}

# Generate a grid of parameters
params <- expand.grid(a = 1:3, b = 4:6)

# Run simulations
results <- runsim(n = 1, grid_params = params, sim_func = sim_function)
print(results)
}
}
