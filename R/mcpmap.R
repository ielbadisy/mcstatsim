#' Parallel Map Over Lists Using Multiple Cores
#'
#' @description This function applies a given function in parallel over elements of a list.
#' It uses `parallel::mcmapply` on Unix-like systems and `parallel::parLapply` on Windows.
#' This approach ensures cross-platform compatibility.
#'
#' @param lists A list of lists where each inner list contains the arguments to be passed to the function `func`.
#' Each inner list should correspond to an argument of `func`, and all lists must have the same length.
#' @param func The function to apply to each set of arguments contained in `lists`.
#' @param num_cores The number of cores to use for parallel execution, defaulting to the number of cores
#' available on the system minus one. On Windows, due to lack of support for multicore execution using
#' forking, the default is set to 1.
#'
#' @return A list of results, each element being the result from applying `func` to the corresponding elements
#' of the input lists.
#' @examples
#' sum_func <- function(x, y) x + y
#' args_list <- list(c(1, 2, 3), c(4, 5, 6))
#' results <- mcpmap(args_list, sum_func, num_cores = 2)
#' @export
#' @importFrom parallel detectCores mcmapply parLapply makeCluster stopCluster clusterExport
#' @seealso \code{\link[parallel]{mcmapply}}, \code{\link[parallel]{parLapply}}
mcpmap <- function(lists, func, num_cores = parallel::detectCores() - 1) {
  stopifnot(is.list(lists))  # Ensure that input is a list of lists

  lengths <- sapply(lists, length)
  if (length(unique(lengths)) != 1) {
    stop("All elements of the list must have the same length")
  }

  # adjust the number of cores for windows
  if (.Platform$OS.type == "windows") {
    num_cores <- 1  # override num_cores to 1 on windows
  }

  do.call(parallel::mcmapply, c(list(FUN = func, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE, mc.cores = num_cores), lists))
}
