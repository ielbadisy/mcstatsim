#' Run simulations in parallel
#'
#' Executes a series of simulations in parallel, based on a set of parameters
#' specified in a dataframe. This function is designed to facilitate large-scale
#' simulation studies by leveraging multiple cores to distribute the computational load.
#'
#' @param n The number of times the simulation function should be executed for each
#' set of parameters. Must be a positive integer.
#' @param grid_params A dataframe where each row corresponds to a unique combination
#' of parameters for the simulation. Typically generated using the `expand.grid()` function.
#' @param sim_func The simulation function to be applied. This function should accept
#' parameters corresponding to a row in `grid_params` and return a dataframe or a list
#' that can be row-bound.
#' @param show_progress Logical indicating whether to display progress messages
#' during the execution of the simulations.
#' @param num_cores The number of cores to use for parallel execution. By default, the
#' number is set to one less than the total number of cores available on the system
#' to leave resources for other processes. On Windows, due to the lack of support for
#' multicore execution using forking, the default is set to 1 regardless of the number
#' of cores available.
#'
#' @return A dataframe combining the results of all simulations, with an additional
#' column to identify the set of parameters (from `grid_params`) used for each simulation.
#'
#' @details This function first validates the input parameters, ensuring that `n` is
#' a positive integer, `grid_params` is a dataframe, and `sim_func` is a valid function.
#' It then uses parallel processing to apply `sim_func` to each combination of parameters
#' specified in `grid_params`, repeating each simulation `n` times. The results are
#' combined into a single dataframe, which is returned to the caller. The parallel
#' execution mode utilized is multicore, through the `parallel` package.
#'
#' @note The multicore parallel mode is only supported on macOS and Linux-based operating
#' systems. Users on other operating systems, such as Windows, may not experience the
#' same parallel processing capabilities and should consider alternative methods for
#' parallel execution.
#'
#' @examples
#' # Define a simple simulation function
#' sim_function <- function(a, b) {
#'   Sys.sleep(0.02) # Simulating a time-consuming process
#'   return(data.frame(result = a + b))
#' }
#'
#' # Generate a grid of parameters
#' params <- expand.grid(a = 1:3, b = 4:6)
#'
#' # Run simulations
#' results <- runsim(n = 2, grid_params = params, sim_func = sim_function, num_cores = 2)
#' print(results)
#'
#' @export
#' @importFrom parallel detectCores
#' @seealso \code{\link[parallel]{mcmapply}} for the underlying parallel apply mechanism.
#'
runsim <- function(n, grid_params, sim_func, show_progress = TRUE, num_cores = parallel::detectCores()-1) {

  # validate input parameters for robustness
  if (!is.numeric(n) || n <= 0 || n != as.integer(n)) {
    stop("'n' must be a positive integer.")
  }

  if (!is.data.frame(grid_params)) {
    stop("'grid_params' must be a data frame, typically the output of expand.grid().")
  }

  if (!is.function(sim_func)) {
    stop("'sim_func' must be a function.")
  }


  # adjust num_cores based on OS
  if (is.null(num_cores)) {  # If not explicitly specified
    if (.Platform$OS.type == "windows") {
      num_cores <- 1  # override num_cores to 1 on Windows
    } else {
      num_cores <- parallel::detectCores() - 1
    }
  }

  if (show_progress) {
    cat("Using", num_cores, "core(s) for parallel simulations:\nStarting simulations...\n")
  }

  tryCatch({
    res <- as.list(replicater(n, expr = {mcpmap(lists = grid_params, func = sim_func, num_cores = num_cores)}, show_progress = show_progress))
  }, error = function(e) {
    cat("An error occurred during simulation: ", e$message, "\n")
    #return(NULL)
  })

  combined_results <- combine_df(res)

  if (show_progress && exists("combined_results")) {
    cat("Simulations complete.\n")
  }

  return(combined_results)
}
