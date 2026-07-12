#' Parallel map over equal-length inputs using base \pkg{parallel}
#'
#' This function applies a user-supplied function over aligned elements of a list
#' or data frame. It uses a PSOCK cluster so it works across operating systems
#' and can execute arbitrary R code in each worker process. When requested, it
#' prints a text progress bar from the master process while workers run.
#'
#' @param lists A list or data frame containing the parameters for the function.
#' @param func The function to be applied.
#' @param num_cores The number of worker processes to use. Defaults to one less
#' than the number of detected cores.
#' @param show_progress Logical indicating whether to print progress messages and
#' display a progress bar.
#' @return A list of results from applying the function over the parameters.
#' @examples
#' params <- list(a = 1:3, b = 4:6)
#' mcpmap(params, function(a, b) a + b, num_cores = 2, show_progress = FALSE)
#' @export
mcpmap <- function(lists, func, num_cores = parallel::detectCores() - 1L, show_progress = TRUE) {
  if (!is.list(lists)) {
    stop("'lists' must be a list or data frame.")
  }

  lengths <- lengths(lists)
  if (length(lengths) == 0L) {
    return(list())
  }

  if (length(unique(lengths)) != 1L) {
    stop("All elements of 'lists' must have the same length.")
  }

  n_tasks <- lengths[[1]]
  if (n_tasks == 0L) {
    return(list())
  }

  cores <- as.integer(num_cores)
  if (is.na(cores) || cores < 1L) {
    cores <- 1L
  }
  cores <- min(cores, n_tasks)

  task_fun <- function(i) {
    args <- lapply(lists, function(x) x[[i]])
    do.call(func, args)
  }

  if (show_progress) {
    cat(sprintf("Dispatching %d task(s) across %d core(s)\n", n_tasks, cores))
  }

  if (cores == 1L) {
    result <- lapply(seq_len(n_tasks), task_fun, lists = lists, func = func)
  } else {
    cl <- parallel::makeCluster(cores, type = "PSOCK")
    on.exit(parallel::stopCluster(cl), add = TRUE)

    attached_pkgs <- setdiff(
      sub("^package:", "", grep("^package:", search(), value = TRUE)),
      c("base", "stats", "graphics", "grDevices", "utils", "datasets", "methods")
    )
    if (length(attached_pkgs) > 0L) {
      parallel::clusterCall(cl, function(pkgs) {
        invisible(lapply(pkgs, require, character.only = TRUE))
      }, attached_pkgs)
    }

    parallel::clusterExport(cl, c("lists", "func", "task_fun"), envir = environment())
    result <- parallel::parLapply(cl, seq_len(n_tasks), task_fun)
  }

  if (show_progress) {
    cat("Completed", n_tasks, "task(s)\n")
  }

  result
}
