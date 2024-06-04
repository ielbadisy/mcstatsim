#' Parallel Map Over a List Using Multiple Coers
#'
#' This function applies a given function in parallel over elements of a list. It uses `parallel::mcmapply` on Unix-like
#' systems and `parallel::parLapply` on Windows. This approach ensures cross-platform compatibility by automatically
#' adjusting to the appropriate method of parallel execution depending on the operating system.
#'
#' @param lists A list of lists where each inner list contains the arguments to be passed to the function `func`.
#'              Each inner list should correspond to an argument of `func`, and all lists must have the same length.
#' @param func The function to apply to each set of arguments contained in `lists`. The function should be capable
#'             of handling input as specified in `lists`.
#' @param num_cores The number of cores to use for parallel execution. This defaults to one less than the total number
#'                  of cores available on the system, leaving resources free for other processes.
#' @return A list of results, where each element is the result from applying `func` to the corresponding elements of
#'         the input lists. The length of the return list is the same as the length of the input lists.
#' @examples
#' # Define a function to be applied
#' sum_func <- function(x, y) x + y
#'
#' # Create a list of arguments
#' args_list <- list(c(1,2,3), to = c(4,5,6))
#'
#' # Apply the function in parallel
#' results <- mcpmap(args_list, sum_func, num_cores = 2)
#' print(results)
#' @export
#' @importFrom parallel detectCores mcmapply makeCluster stopCluster clusterExport parLapply
#' @keywords parallel
#' @note On Windows systems, since `mcmapply` is not supported due to the lack of forking ability, this function
#'       uses a parallel socket cluster (`parLapply`) instead. Users are advised that parallel execution on Windows
#'       involves creating and managing a cluster, which might introduce some overhead compared to forking on Unix-like systems.
mcpmap <- function(lists, func, num_cores = parallel::detectCores()-1) {
  stopifnot(is.list(lists))  # Ensure that input is a list of lists

  lengths <- sapply(lists, length)
  if (length(unique(lengths)) != 1) {
    stop("All elements of the list must have the same length")
  }

  is_windows <- .Platform$OS.type == "windows"

  if (is_windows) {
    # On Windows, use parLapply with a PSOCK cluster
    cl <- parallel::makeCluster(num_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)  # Ensure cluster is stopped when function exits

    # Prepare list of arguments for each call
    args <- transpose(lists)

    # Export the function and any required variables
    parallel::clusterExport(cl, varlist = "func", envir = environment())

    # Apply function over the list using parLapply
    results <- parallel::parLapply(cl, args, function(x) do.call(func, x))
  } else {
    # On Unix-like systems, use mcmapply
    results <- do.call(parallel::mcmapply, c(list(FUN = func, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE, mc.cores = num_cores), lists))
  }

  return(results)
}

# Helper function to transpose a list of vectors into a list of lists for each combination
transpose <- function(lst) {
  mapply(function(...) list(...), lst, SIMPLIFY = FALSE)
}
