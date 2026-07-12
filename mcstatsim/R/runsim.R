#' Run Monte Carlo Simulations in Parallel
#'
#' This function executes a series of Monte Carlo simulations in parallel, providing
#' detailed progress updates and a text progress bar when `show_progress = TRUE`.
#'
#' @param n The number of times the simulation function should be executed for each set of parameters. Must be a positive integer.
#' @param grid_params A dataframe where each row corresponds to a unique combination of parameters for the simulation. Typically generated using `expand.grid`.
#' @param sim_func The simulation function to be applied. This function should accept parameters corresponding to a row in `grid_params` and return a dataframe or a list that can be row-bound.
#' @param show_progress Logical indicating whether to display progress messages during the execution of the simulations.
#' @param num_cores The number of cores to use for parallel execution. The default is one less than the total number of cores available on the system.
#' @return A combined dataframe of all simulation results.
#' @details The function first validates the input parameters. It then expands the
#' simulation grid across `n` replications and dispatches the full task set to
#' `mcpmap()` in one parallel call. The results are then regrouped by replication
#' and combined into a single data frame. This design is intended for heavy Monte
#' Carlo workloads where process-based parallelism amortizes cluster overhead.
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
  }

  if (nrow(grid_params) == 0L) {
    stop("'grid_params' must contain at least one row.")
  }

  start_time <- Sys.time()

  repeated_grid <- data.table::rbindlist(rep(list(grid_params), n), use.names = TRUE, fill = TRUE)
  rep_id <- rep(seq_len(n), each = nrow(grid_params))
  task_args <- lapply(data.table::transpose(repeated_grid), as.list)
  tasks <- Map(function(args, id) {
    args$.mcstatsim_id <- id
    args
  }, task_args, rep_id)

  task_runner <- function(task) {
    id <- task$.mcstatsim_id
    task$.mcstatsim_id <- NULL
    out <- do.call(sim_func, task)
    if (!is.data.frame(out)) {
      out <- as.data.frame(out)
    }
    out$ID <- id
    out
  }

  raw_results <- parallel_task_map(tasks, task_runner, num_cores = num_cores, show_progress = show_progress)

  if (is.null(raw_results)) {
    stop("Failed to obtain simulation results.")
  }

  combined_results <- data.table::rbindlist(raw_results, use.names = TRUE, fill = TRUE)

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
