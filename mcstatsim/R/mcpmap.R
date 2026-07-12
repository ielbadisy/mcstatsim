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
parallel_task_map <- function(tasks, func, num_cores = parallel::detectCores() - 1L, show_progress = TRUE) {
  if (!is.list(tasks)) {
    stop("'tasks' must be a list.")
  }

  n_tasks <- length(tasks)
  if (n_tasks == 0L) {
    return(list())
  }

  cores <- as.integer(num_cores)
  if (is.na(cores) || cores < 1L) {
    cores <- 1L
  }
  cores <- min(cores, n_tasks)

  if (show_progress) {
    cat(sprintf("Dispatching %d task(s) across %d core(s)\n", n_tasks, cores))
  }

  if (cores == 1L) {
    pb <- if (show_progress) utils::txtProgressBar(min = 0, max = n_tasks, style = 3) else NULL
    if (show_progress) on.exit(close(pb), add = TRUE)

    result <- vector("list", n_tasks)
    for (i in seq_len(n_tasks)) {
      result[[i]] <- func(tasks[[i]])
      if (show_progress) {
        utils::setTxtProgressBar(pb, i)
      }
    }
  } else if (!show_progress && .Platform$OS.type != "windows") {
    result <- parallel::mclapply(tasks, func, mc.cores = cores, mc.preschedule = TRUE)
  } else {
    chunk_size <- max(1L, ceiling(n_tasks / (cores * 4L)))
    chunk_ids <- split(seq_len(n_tasks), ceiling(seq_len(n_tasks) / chunk_size))
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

    parallel::clusterExport(cl, c("tasks", "func"), envir = environment())
    result <- vector("list", n_tasks)
    pb <- if (show_progress) utils::txtProgressBar(min = 0, max = n_tasks, style = 3) else NULL
    if (show_progress) on.exit(close(pb), add = TRUE)

    done <- 0L
    for (chunk in chunk_ids) {
      chunk_res <- parallel::parLapply(cl, chunk, function(i) func(tasks[[i]]))
      result[chunk] <- chunk_res
      done <- done + length(chunk)
      if (show_progress) {
        utils::setTxtProgressBar(pb, done)
      }
    }
  }

  if (show_progress) {
    cat("\nCompleted", n_tasks, "task(s)\n")
  }

  result
}

mcpmap <- function(lists, func, num_cores = parallel::detectCores() - 1L, show_progress = TRUE) {
  if (is.data.frame(lists)) {
    lists <- lapply(data.table::transpose(lists), as.list)
  }

  if (!is.list(lists)) {
    stop("'lists' must be a list or data frame.")
  }

  if (length(lists) == 0L) {
    return(list())
  }

  if (is.data.frame(lists[[1]]) || is.list(lists[[1]])) {
    tasks <- lists
  } else {
    lengths <- lengths(lists)
    if (length(unique(lengths)) != 1L) {
      stop("All elements of 'lists' must have the same length.")
    }
    tasks <- lapply(data.table::transpose(lists), as.list)
  }

  task_runner <- function(task) {
    do.call(func, task)
  }

  parallel_task_map(tasks, task_runner, num_cores = num_cores, show_progress = show_progress)
}
