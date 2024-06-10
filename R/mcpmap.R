#' @title Parallel Map Over a List Using Multiple Cores
#' @description This function applies a given function in parallel over elements of a list.
#' It uses `parallel::mcmapply` on Unix-like systems and `parallel::parLapply` on Windows,
#' ensuring cross-platform compatibility.
#'
#' @param lists A list of lists where each inner list contains the arguments to be passed to the function `func`.
#' Each inner list should correspond to an argument of `func`, and all lists must have the same length.
#' @param func The function to apply to each set of arguments contained in `lists`.
#' @param num_cores The number of cores to use for parallel execution. This defaults to one less
#' than the total number of cores available on the system, leaving resources free for other processes.
#' Note: On Windows, `num_cores` is automatically set to 1 due to limitations in parallel processing support.
#' @return A list of results, where each element is the result from applying `func` to the corresponding elements of
#' the input lists.
#' @examples
#' sum_func <- function(x, y) x + y
#' args_list <- list(c(1,2,3), c(4,5,6))
#' results <- mcpmap(args_list, sum_func, num_cores = 2)
#' print(results)
#' @export
#' @importFrom parallel detectCores mcmapply makeCluster stopCluster clusterExport parLapply
#' @keywords parallel

mcpmap <- function(lists, func, num_cores = parallel::detectCores() - 1) {
  stopifnot(is.list(lists))  # Ensure that input is a list of lists
  lengths <- sapply(lists, length)
  if (length(unique(lengths)) != 1) {
    stop("All elements of the list must have the same length.")
  }

  # Adjust the number of cores for Windows due to lack of forking support
  if (.Platform$OS.type == "windows") {
    num_cores <- 1  # Override num_cores to 1 on Windows
    cl <- parallel::makeCluster(num_cores)
    on.exit(parallel::stopCluster(cl))  # Ensure cluster is stopped when function exits

    # Prepare list of arguments for each call
    args <- transpose(lists)

    # Export the function and any required variables
    parallel::clusterExport(cl, varlist = c("func"), envir = environment())

    # Apply function over the list using parLapply
    results <- parallel::parLapply(cl, args, function(x) do.call(func, x))
  } else {
    # On Unix-like systems, use mcmapply
    results <- do.call(parallel::mcmapply, c(list(FUN = func, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE, mc.cores = num_cores), lists))
  }

  return(results)
}


# transpose a list of vectors into a list of lists for each combination
transpose <- function(lst) {
  mapply(function(...) list(...), lst, SIMPLIFY = FALSE)
}


