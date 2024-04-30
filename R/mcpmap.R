#' Parallel map over a list using multiple cores
#'
#' Applies a function in parallel over elements of a list of elements with the same length with support for multiple cores.
#' The function uses `parallel::mcmapply` under the hood to perform the parallel computation, automatically determining the number
#' of cores to use if not specified.
#'
#' @param lists A list of lists containing the arguments to pass to the function `func`. Each inner list should
#' correspond to an argument of `func`, and all lists must be of the same length.
#' @param func The function to apply to each set of arguments contained in `lists`.
#' @param num_cores The number of cores to use for parallel execution. The default is
#' one less than the total number of cores available on the system to leave resources
#' for other processes.
#' @return The result of applying `func` to the elements of `lists` in parallel. The return value is a list of the
#' same length as the input lists, where each element is the result of applying `func` to the corresponding elements
#' of the input lists.
#' @examples
#' # Define a function to apply
#' sum_func <- function(x, y) x + y
#'
#' # Create a list of arguments
#' args_list <- list(c(1,2,3), c(4,5,6))
#'
#' # Apply the function in parallel
#' results <- mcpmap(args_list, sum_func, num_cores = 2)
#' print(results)
#' @export
#' @importFrom parallel mcmapply detectCores
#'
mcpmap <- function(lists, func, num_cores = parallel::detectCores()-1) {
  stopifnot(is.list(lists))  # Ensure that input is a list of lists

  lengths <- sapply(lists, length)
  if (length(unique(lengths)) != 1) {
    stop("All elements of the list must have the same length")
  }

  do.call(parallel::mcmapply, c(list(FUN = func, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE, mc.cores = num_cores), lists))
}


