#' Run Monte Carlo Simulations in Parallel
#'
#' This function executes a series of Monte Carlo simulations in parallel, providing detailed progress updates.
#'
#' @param n The number of times the simulation function should be executed for each set of parameters. Must be a positive integer.
#' @param grid_params A dataframe where each row corresponds to a unique combination of parameters for the simulation. Typically generated using `expand.grid`.
#' @param sim_func The simulation function to be applied. This function should accept parameters corresponding to a row in `grid_params` and return a dataframe or a list that can be row-bound.
#' @param show_progress Logical indicating whether to display progress messages during the execution of the simulations.
#' @param num_cores The number of cores to use for parallel execution. The default is one less than the total number of cores available on the system.
#' @return A combined dataframe of all simulation results.
#' @details The function first validates the input parameters. It then uses parallel processing to apply `sim_func` to each combination of parameters specified in `grid_params`, repeating each simulation `n` times.
#' The results are combined into a single dataframe.
#' @examples
#' \dontrun{
#' library(mcstatsim)
#'
#' # Define a simple simulation function
#' sim_function <- function(a, b) {
#'   Sys.sleep(0.1)  # Simulate a time-consuming process
#'   return(data.frame(result = a + b))
#' }
#'
#' # Generate a grid of parameters
#' params <- expand.grid(a = 1:3, b = 4:6)
#'
#' # Run simulations
#' results <- runsim(n = 1, grid_params = params, sim_func = sim_function)
#' print(results)
#' }
#' @importFrom parallel detectCores
#' @importFrom utils object.size
#' @export
runsim <- function(n, grid_params, sim_func, show_progress = TRUE, num_cores = parallel::detectCores() - 1) {

  # input validation
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("'n' must be a positive integer.")
  }

  if (!is.data.frame(grid_params)) {
    stop("'grid_params' must be a data frame, typically the output of expand.grid().")
  }

  if (!is.function(sim_func)) {
    stop("'sim_func' must be a function.")
  }

  if (show_progress) {
    cat("Using", num_cores, "core(s) for parallel simulations...\n")
    cat("Date:", format(Sys.time(), "%Y-%m-%d"), "\n")
    cat("Starting simulations at", format(Sys.time(), "%X"), "\n")
  }

  # detailed progress
  start_time <- Sys.time()
  res <- lapply(seq_len(n), function(i) {
    iter_start_time <- Sys.time()

    if (show_progress) {
      cat(sprintf("Iteration %d/%d at %s\n", i, n, format(iter_start_time, "%X")))
    }

    # simulation engine
    result <- mcpmap(lists = grid_params, func = sim_func, num_cores = num_cores, show_progress = show_progress)

    return(result)
  })

  if (is.null(res)) {
    stop("Failed to obtain simulation results during replication.")
  }

  combined_results <- combine_df(res)

  if (show_progress) {
    total_elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
    cat("All simulations complete at", format(Sys.time(), "%X"), "\n")
    cat("Date:", format(Sys.time(), "%Y-%m-%d"), "\n")
    cat(sprintf("Total elapsed time: %02d:%02d:%02d\n",
                floor(total_elapsed_time / 3600), floor((total_elapsed_time %% 3600) / 60), floor(total_elapsed_time %% 60)))
    cat(sprintf("Final memory usage: %.2f MB\n", object.size(combined_results) / (1024^2)))
  }

  return(combined_results)
}
