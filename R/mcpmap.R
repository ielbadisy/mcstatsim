#' Parallel Map Over a List Using Multiple Coers
#'
<<<<<<< HEAD
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
=======
#' Applies a function in parallel over elements of a list with the same length with support for multiple cores.
#' The function uses `parallel::mcmapply` under the hood to perform the parallel computation, automatically determining the number
#' of cores to use if not specified. On Windows systems, the number of cores is automatically set to 1 due to limitations
#' in parallel processing support.
#'
#' @param lists A list of lists containing the arguments to pass to the function `func`. Each inner list should
#' correspond to an argument of `func`, and all lists must be of the same length.
#' @param func The function to apply to each set of arguments contained in `lists`.
#' @param num_cores The number of cores to use for parallel execution. The default is
#' one less than the total number of cores available on the system to leave resources
#' for other processes. Note: On Windows, `num_cores` is automatically set to 1.
#' @return The result of applying `func` to the elements of `lists` in parallel. The return value is a list of the
#' same length as the input lists, where each element is the result of applying `func` to the corresponding elements
#' of the input lists.
>>>>>>> 99ff399 (override num_cores to 1 on windows)
#' @examples
#' # Define a function to be applied
#' sum_func <- function(x, y) x + y
#'
#' # Create a list of arguments
<<<<<<< HEAD
#' args_list <- list(c(1,2,3), to = c(4,5,6))
=======
#' args followed by the function argument.
#' args_list <- list(c(1,2,3), c(4,5,6))
>>>>>>> 99ff399 (override num_cores to 1 on windows)
#'
#' # Apply the function in parallel
#' results <- mcpmap(args_list, sum_func, num_cores = 2)
#' print(results)
#' @export
<<<<<<< HEAD
#' @importFrom parallel detectCores mcmapply makeCluster stopCluster clusterExport parLapply
#' @keywords parallel
#' @note On Windows systems, since `mcmapply` is not supported due to the lack of forking ability, this function
#'       uses a parallel socket cluster (`parLapply`) instead. Users are advised that parallel execution on Windows
#'       involves creating and managing a cluster, which might introduce some overhead compared to forking on Unix-like systems.
mcpmap <- function(lists, func, num_cores = parallel::detectCores()-1) {
=======
#' @importFrom parallel mcmapply detectCores
#'
mcpmap <- function(lists, func, num_cores = parallel::detectCores() - 1) {
>>>>>>> 99ff399 (override num_cores to 1 on windows)
  stopifnot(is.list(lists))  # Ensure that input is a list of lists

  lengths <- sapply(lists, length)
  if (length(unique(lengths)) != 1) {
    stop("All elements of the list must have the same length")
  }

<<<<<<< HEAD
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
=======
  # adjust the number of cores for windows
  if (.Platform$OS.type == "windows") {
    num_cores <- 1  # override num_cores to 1 on windows
  }

  do.call(parallel::mcmapply, c(list(FUN = func, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE, mc.cores = num_cores), lists))
}

>>>>>>> 99ff399 (override num_cores to 1 on windows)
