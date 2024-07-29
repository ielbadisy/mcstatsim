#' Parallel Map Function using pbapply::pbmapply
#'
#' This function applies a given function over a list of parameters in parallel using multiple cores.
#'
#' @param lists A list of lists containing the parameters for the function.
#' @param func The function to be applied.
#' @param num_cores The number of cores to use for parallel execution. Default is one less than the total number of available cores.
#' @param show_progress Logical indicating whether to display the progress bar. Default is TRUE.
#' @return A list of results from applying the function over the parameters.
#' @details The function ensures that all elements in the list have the same length and uses `pbapply::pbmapply` for parallel processing.
#' It sets the number of cores based on the operating system and then applies the function in parallel.
#' @examples
#' params <- list(a = 1:3, b = 4:6)
#' mcpmap(params, function(a, b) a + b, num_cores = 2)
#' @importFrom pbapply pbmapply pboptions
#' @export
mcpmap <- function(lists, func, num_cores = parallel::detectCores() - 1, show_progress = TRUE) {
  stopifnot(is.list(lists))  # Ensure that input is a list of lists

  lengths <- sapply(lists, length)
  if (length(unique(lengths)) != 1) {
    stop("All elements of the list must have the same length")
  }

  if (show_progress) {
    pbapply::pboptions(type = "timer", use_lb = FALSE, nout = num_cores, char = "=")
  } else {
    pbapply::pboptions(type = "none")
  }

  do.call(pbapply::pbmapply, c(list(FUN = func, MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE), lists))
}
