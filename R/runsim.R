#' Run Monte Carlo Simulations in Parallel
#'
#' This function executes a series of Monte Carlo simulations in parallel, providing detailed progress updates.
#'
#' @param n The number of times the simulation function should be executed for each set of parameters. Must be a positive integer.
#' @param grid_params A dataframe where each row corresponds to a unique combination of parameters for the simulation. Typically generated using `expand.grid`.
#' @param sim_func The simulation function to be applied. This function should accept parameters corresponding to a row in `grid_params` and return a dataframe or a list that can be row-bound.
#' @param show_progress Logical indicating whether to display progress messages during the execution of the simulations.
#' @param num_cores The number of cores to use for parallel execution. The default is one less than the total number of cores available on the system.
#' @return A combined dataframe of all simulation results. Each row carries an `ID` column giving the replication index (1 to `n`) it came from.
#' @details The function first validates the input parameters. It then builds a single flat list of
#' `n * nrow(grid_params)` jobs (replication-major order: all conditions for replication 1, then all
#' conditions for replication 2, and so on) and dispatches it in one load-balanced parallel pass via
#' [mcpmap()]. Results are row-bound into a single dataframe and tagged with the replication index in
#' the `ID` column, preserving the layout produced by earlier versions of the package.
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
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("'n' must be a positive integer.")
  }

  if (!is.data.frame(grid_params)) {
    stop("'grid_params' must be a data frame, typically the output of expand.grid().")
  }

  if (!is.function(sim_func)) {
    stop("'sim_func' must be a function.")
  }

  n <- as.integer(n)
  n_cond <- nrow(grid_params)

  if (n_cond < 1L) {
    stop("'grid_params' must have at least one row.")
  }

  n_jobs <- n * n_cond

  if (show_progress) {
    cat("Using", num_cores, "core(s) for parallel simulations...\n")
    cat("Date:", format(Sys.time(), "%Y-%m-%d"), "\n")
    cat(sprintf("Dispatching %d job(s): %d replication(s) x %d condition(s)\n", n_jobs, n, n_cond))
    cat("Starting simulations at", format(Sys.time(), "%X"), "\n")
  }

  start_time <- Sys.time()

  # replication-major flat job list: each grid column recycled n times so that
  # jobs run as (rep 1: cond 1..K), (rep 2: cond 1..K), ...
  param_names <- names(grid_params)
  job_args <- lapply(param_names, function(nm) rep(grid_params[[nm]], times = n))
  names(job_args) <- param_names
  rep_index <- rep(seq_len(n), each = n_cond)

  # simulation engine: one load-balanced parallel pass over every job
  results <- mcpmap(lists = job_args, func = sim_func, num_cores = num_cores, show_progress = show_progress)

  if (is.null(results) || length(results) != n_jobs) {
    stop("Failed to obtain simulation results for every job.")
  }

  is_df <- vapply(results, is.data.frame, logical(1))
  if (!all(is_df)) {
    stop("'sim_func' must return a data frame for every job; ",
         sum(!is_df), " of ", n_jobs, " job(s) returned something else.")
  }

  row_counts <- vapply(results, nrow, integer(1))
  combined_results <- do.call(rbind, results)
  combined_results$ID <- rep(rep_index, times = row_counts)
  rownames(combined_results) <- NULL

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
