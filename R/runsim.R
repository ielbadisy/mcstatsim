#' Run Monte Carlo Simulations in Parallel
#'
#' This function executes a series of Monte Carlo simulations in parallel, providing detailed progress updates.
#'
#' @param n The number of times the simulation function should be executed for each set of parameters. Must be a positive integer.
#' @param grid_params A dataframe where each row corresponds to a unique combination of parameters for the simulation. Typically generated using `expand.grid`.
#' @param sim_func The simulation function to be applied. This function should accept parameters corresponding to a row in `grid_params` and return a dataframe or a list that can be row-bound.
#' @param show_progress Logical indicating whether to display progress messages during the execution of the simulations.
#' @param num_cores The number of cores to use for parallel execution. The default is one less than the total number of cores available on the system.
#' @param seed Optional single number. When supplied, every job is assigned its own
#'   independent \code{"L'Ecuyer-CMRG"} random-number stream derived from `seed`, so the
#'   whole run is reproducible and the result does not depend on `num_cores` or on the
#'   order in which jobs finish. When `NULL` (default) the session RNG is left untouched.
#' @param on_error How to handle a job in which `sim_func` throws an error. `"stop"`
#'   (default) aborts the whole run, reproducing the behaviour of earlier versions.
#'   `"warn"` drops the failed jobs, keeps the rest, and emits a warning. `"omit"` drops
#'   the failed jobs silently. With `"warn"` or `"omit"`, details of every failed job are
#'   stored in `attr(x, "errors")`.
#' @param save_path Optional path to an `.rds` file used as a checkpoint. When set,
#'   `runsim()` runs the replications in chunks and rewrites this file after each chunk,
#'   so a run that is interrupted (crash, time limit, `Ctrl-C`) can be resumed simply by
#'   calling `runsim()` again with the same `save_path` and matching `n` / `grid_params` /
#'   `seed`: completed replications are loaded from disk and skipped. When `NULL`
#'   (default) nothing is written and all jobs run in a single pass.
#' @param checkpoint_every Positive integer: how many replications to run between
#'   checkpoint writes when `save_path` is set. Larger values mean less I/O but more
#'   lost work if the run is interrupted. Ignored when `save_path` is `NULL`.
#' @return A combined dataframe of all simulation results. Every `grid_params` column that
#'   `sim_func` did not itself return is prepended, so each row identifies its own
#'   condition, and an `ID` column gives the replication index (1 to `n`). When `seed` is
#'   supplied it is stored in `attr(x, "seed")`. Any warnings emitted by `sim_func` are
#'   collected in `attr(x, "warnings")`, and, when `on_error` is not `"stop"`, failed jobs
#'   are described in `attr(x, "errors")` (see [failure_summary()]).
#' @details The function first validates the input parameters. It then builds a single flat list of
#' `n * nrow(grid_params)` jobs (replication-major order: all conditions for replication 1, then all
#' conditions for replication 2, and so on) and dispatches it in one load-balanced parallel pass via
#' [mcpmap()]. Results are row-bound into a single dataframe, the condition's `grid_params`
#' columns are prepended (unless `sim_func` already returned a column of that name), and the
#' replication index is tagged in the `ID` column.
#'
#' If `seed` is supplied, `n * nrow(grid_params)` independent RNG streams are generated up front with
#' [parallel::nextRNGStream()] and handed to the jobs by position. Because job positions are fixed
#' (replication-major), a given `(replication, condition)` cell always draws from the same stream
#' regardless of core count, so runs are bit-for-bit reproducible. Do not call `set.seed()` inside
#' `sim_func` when using `seed`, as that would defeat the per-stream design.
#'
#' Each job is wrapped so that an error in `sim_func` is caught rather than lost in a worker
#' process, and any warnings it emits are recorded. See `on_error` for how failures are handled.
#'
#' When `save_path` is set the replications are run in chunks of `checkpoint_every` and the
#' checkpoint file is rewritten after each chunk. Per-job RNG streams are keyed by position, so a
#' resumed run produces exactly the same results as an uninterrupted one.
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
#' @importFrom parallel detectCores nextRNGStream
#' @importFrom utils object.size
#' @export
runsim <- function(n, grid_params, sim_func, show_progress = TRUE, num_cores = parallel::detectCores() - 1, seed = NULL,
                   on_error = c("stop", "warn", "omit"), save_path = NULL, checkpoint_every = 1L) {

  on_error <- match.arg(on_error)

  # input validation
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop("'n' must be a positive integer.")
  }

  if (!is.data.frame(grid_params)) {
    stop("'grid_params' must be a data frame, typically the output of expand.grid().")
  }

  if (!is.function(sim_func)) {
    stop("'sim_func' must be a function.")
  }

  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1 || is.na(seed))) {
    stop("'seed' must be a single number or NULL.")
  }

  if (!is.null(save_path) && (!is.character(save_path) || length(save_path) != 1 || is.na(save_path))) {
    stop("'save_path' must be a single file path or NULL.")
  }

  if (!is.numeric(checkpoint_every) || length(checkpoint_every) != 1 ||
      checkpoint_every <= 0 || checkpoint_every != as.integer(checkpoint_every)) {
    stop("'checkpoint_every' must be a positive integer.")
  }

  n <- as.integer(n)
  n_cond <- nrow(grid_params)
  checkpoint_every <- as.integer(checkpoint_every)

  if (n_cond < 1L) {
    stop("'grid_params' must have at least one row.")
  }

  n_jobs <- n * n_cond

  if (show_progress) {
    cat("Using", num_cores, "core(s) for parallel simulations...\n")
    cat("Date:", format(Sys.time(), "%Y-%m-%d"), "\n")
    cat(sprintf("Dispatching %d job(s): %d replication(s) x %d condition(s)\n", n_jobs, n, n_cond))
    cat("Starting simulations at", format(Sys.time(), "%X"), "\n")
  }

  start_time <- Sys.time()

  # replication-major flat job list: job k holds the argument list for
  # (replication rep_index[k], condition cond_index[k]); jobs run as
  # (rep 1: cond 1..K), (rep 2: cond 1..K), ...
  param_names <- names(grid_params)
  cond_index <- rep(seq_len(n_cond), times = n)
  rep_index <- rep(seq_len(n), each = n_cond)
  jobs <- lapply(cond_index, function(ci) {
    args <- lapply(param_names, function(nm) grid_params[[nm]][[ci]])
    names(args) <- param_names
    args
  })

  # one independent L'Ecuyer-CMRG stream per job, assigned by position
  streams <- NULL
  if (!is.null(seed)) {
    # snapshot and restore the caller's RNG state so runsim() has no side effect
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    saved_seed <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE)
    saved_kind <- RNGkind()
    on.exit({
      RNGkind(saved_kind[1], saved_kind[2])
      if (had_seed) assign(".Random.seed", saved_seed, envir = globalenv())
      else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())
    }, add = TRUE)

    RNGkind("L'Ecuyer-CMRG")
    set.seed(as.integer(seed))
    streams <- vector("list", n_jobs)
    state <- .Random.seed
    for (k in seq_len(n_jobs)) {
      streams[[k]] <- state
      state <- parallel::nextRNGStream(state)
    }
  }

  # each job returns an envelope so a worker-side error is carried back rather
  # than lost, and warnings are captured before the backend can suppress them
  run_one <- function(job, stream = NULL) {
    if (!is.null(stream)) {
      RNGkind("L'Ecuyer-CMRG")
      assign(".Random.seed", stream, envir = globalenv())
    }
    warns <- character()
    value <- withCallingHandlers(
      tryCatch(
        do.call(sim_func, job),
        error = function(e) structure(list(message = conditionMessage(e)), class = "runsim_failed_job")
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warns)
  }

  dispatch <- function(job_idx) {
    map_lists <- list(job = jobs[job_idx])
    if (!is.null(streams)) map_lists$stream <- streams[job_idx]
    out <- mcpmap(lists = map_lists, func = run_one, num_cores = num_cores, show_progress = show_progress)
    if (is.null(out) || length(out) != length(job_idx)) {
      stop("Failed to obtain simulation results for every job.")
    }
    out
  }

  # per-job envelopes for the whole run, filled in as chunks complete
  values <- vector("list", n_jobs)
  warns <- rep(list(character()), n_jobs)

  if (is.null(save_path)) {
    res <- dispatch(seq_len(n_jobs))
    values <- lapply(res, `[[`, "value")
    warns <- lapply(res, `[[`, "warnings")
  } else {
    ck <- .runsim_checkpoint_load(save_path, n = n, n_cond = n_cond, seed = seed,
                                  grid_params = grid_params, sim_func = sim_func)
    done_reps <- integer(0)
    if (!is.null(ck)) {
      values[ck$done_jobs] <- ck$values[ck$done_jobs]
      warns[ck$done_jobs] <- ck$warns[ck$done_jobs]
      done_reps <- ck$done_reps
      if (show_progress && length(done_reps)) {
        cat(sprintf("Resuming from %s: %d of %d replication(s) already done.\n",
                    save_path, length(done_reps), n))
      }
    }

    todo_reps <- setdiff(seq_len(n), done_reps)
    chunks <- split(todo_reps, ceiling(seq_along(todo_reps) / checkpoint_every))

    for (chunk in chunks) {
      job_idx <- which(rep_index %in% chunk)
      res <- dispatch(job_idx)
      chunk_values <- lapply(res, `[[`, "value")
      chunk_warns <- lapply(res, `[[`, "warnings")

      if (on_error == "stop") {
        chunk_failed <- vapply(chunk_values, inherits, logical(1), what = "runsim_failed_job")
        if (any(chunk_failed)) {
          # keep only the chunks that fully succeeded; this one is retried on resume
          .runsim_checkpoint_save(save_path, n = n, n_cond = n_cond, seed = seed,
                                  grid_params = grid_params, sim_func = sim_func,
                                  values = values, warns = warns, done_reps = done_reps)
          stop(sprintf("'sim_func' errored (on_error = \"stop\"). First error: %s\nProgress saved to %s.",
                       chunk_values[chunk_failed][[1]]$message, save_path))
        }
      }

      values[job_idx] <- chunk_values
      warns[job_idx] <- chunk_warns
      done_reps <- sort(c(done_reps, chunk))
      .runsim_checkpoint_save(save_path, n = n, n_cond = n_cond, seed = seed,
                              grid_params = grid_params, sim_func = sim_func,
                              values = values, warns = warns, done_reps = done_reps)
      if (show_progress) {
        cat(sprintf("Checkpoint saved: %d/%d replication(s) complete.\n", length(done_reps), n))
      }
    }
  }

  combined_results <- .runsim_assemble(values = values, warns = warns,
                                       cond_index = cond_index, rep_index = rep_index,
                                       grid_params = grid_params, n_jobs = n_jobs,
                                       on_error = on_error, seed = seed)

  if (show_progress) {
    total_elapsed_time <- as.numeric(Sys.time() - start_time, units = "secs")
    n_failed <- nrow(attr(combined_results, "errors"))
    if (!is.null(n_failed) && n_failed > 0) {
      cat(sprintf("Dropped %d failed job(s); see attr(, \"errors\").\n", n_failed))
    }
    cat("All simulations complete at", format(Sys.time(), "%X"), "\n")
    cat("Date:", format(Sys.time(), "%Y-%m-%d"), "\n")
    cat(sprintf("Total elapsed time: %02d:%02d:%02d\n",
                floor(total_elapsed_time / 3600), floor((total_elapsed_time %% 3600) / 60), floor(total_elapsed_time %% 60)))
    cat(sprintf("Final memory usage: %.2f MB\n", object.size(combined_results) / (1024^2)))
  }

  return(combined_results)
}


# Turn per-job envelopes into the combined result data frame, applying the
# on_error policy and attaching the "errors"/"warnings"/"seed" attributes.
.runsim_assemble <- function(values, warns, cond_index, rep_index, grid_params, n_jobs, on_error, seed) {
  failed <- vapply(values, inherits, logical(1), what = "runsim_failed_job")

  if (any(failed)) {
    errors_df <- data.frame(
      grid_params[cond_index[failed], , drop = FALSE],
      ID = rep_index[failed],
      error = vapply(values[failed], function(v) v$message, character(1)),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    if (on_error == "stop") {
      stop(sprintf("'sim_func' errored on %d of %d job(s). First error: %s",
                   sum(failed), n_jobs, errors_df$error[1]))
    }
    if (on_error == "warn") {
      warning(sprintf("'sim_func' errored on %d of %d job(s); those cells were dropped. See attr(, \"errors\").",
                      sum(failed), n_jobs))
    }
  } else {
    errors_df <- NULL
  }

  keep <- !failed
  ok_values <- values[keep]

  if (length(ok_values) == 0L) {
    stop(sprintf("'sim_func' errored on all %d job(s). First error: %s",
                 n_jobs, errors_df$error[1]))
  }

  is_df <- vapply(ok_values, is.data.frame, logical(1))
  if (!all(is_df)) {
    stop("'sim_func' must return a data frame for every job; ",
         sum(!is_df), " of ", length(ok_values), " successful job(s) returned something else.")
  }

  row_counts <- vapply(ok_values, nrow, integer(1))
  combined_results <- do.call(rbind, ok_values)
  combined_results$ID <- rep(rep_index[keep], times = row_counts)

  # prepend the condition's grid columns (those sim_func did not already return),
  # so the result identifies its own conditions for aggregate()/summarise_sim()
  new_cols <- setdiff(names(grid_params), names(combined_results))
  if (length(new_cols)) {
    kept_cond <- rep(cond_index[keep], times = row_counts)
    combined_results <- cbind(
      grid_params[kept_cond, new_cols, drop = FALSE],
      combined_results,
      stringsAsFactors = FALSE
    )
  }
  rownames(combined_results) <- NULL

  has_warn <- lengths(warns) > 0L
  if (any(has_warn)) {
    warn_index <- rep(which(has_warn), times = lengths(warns[has_warn]))
    attr(combined_results, "warnings") <- data.frame(
      grid_params[cond_index[warn_index], , drop = FALSE],
      ID = rep_index[warn_index],
      warning = unlist(warns[has_warn], use.names = FALSE),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
  if (!is.null(errors_df)) attr(combined_results, "errors") <- errors_df
  if (!is.null(seed)) attr(combined_results, "seed") <- seed

  combined_results
}


# Signature the checkpoint must match before a resume is allowed.
.runsim_run_key <- function(n, n_cond, seed, grid_params, sim_func) {
  list(
    n = n,
    n_cond = n_cond,
    seed = seed,
    grid = grid_params,
    sim_formals = names(formals(sim_func))
  )
}

.runsim_checkpoint_save <- function(path, n, n_cond, seed, grid_params, sim_func, values, warns, done_reps) {
  obj <- list(
    mcstatsim_checkpoint = 1L,
    key = .runsim_run_key(n, n_cond, seed, grid_params, sim_func),
    values = values,
    warns = warns,
    done_reps = done_reps
  )
  tmp <- paste0(path, ".tmp")
  saveRDS(obj, tmp)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

.runsim_checkpoint_load <- function(path, n, n_cond, seed, grid_params, sim_func) {
  if (!file.exists(path)) return(NULL)
  obj <- readRDS(path)
  if (!is.list(obj) || !identical(obj$mcstatsim_checkpoint, 1L)) {
    stop("'save_path' exists but is not an mcstatsim checkpoint file: ", path)
  }

  key <- .runsim_run_key(n, n_cond, seed, grid_params, sim_func)
  mismatched <- character()
  if (!identical(key$n, obj$key$n)) mismatched <- c(mismatched, "n")
  if (!identical(key$n_cond, obj$key$n_cond)) mismatched <- c(mismatched, "grid_params (row count)")
  if (!identical(key$seed, obj$key$seed)) mismatched <- c(mismatched, "seed")
  if (!isTRUE(all.equal(key$grid, obj$key$grid))) mismatched <- c(mismatched, "grid_params")
  if (length(mismatched)) {
    stop("checkpoint at ", path, " does not match this call (differs in: ",
         paste(mismatched, collapse = ", "),
         "). Delete the file or use a different 'save_path' to start fresh.")
  }
  if (!identical(key$sim_formals, obj$key$sim_formals)) {
    warning("checkpoint at ", path, " was written with a 'sim_func' that had different ",
            "arguments; resuming anyway, but mixed results may be inconsistent.")
  }

  done_reps <- obj$done_reps
  done_jobs <- which(rep(seq_len(n), each = n_cond) %in% done_reps)
  list(values = obj$values, warns = obj$warns, done_reps = done_reps, done_jobs = done_jobs)
}
