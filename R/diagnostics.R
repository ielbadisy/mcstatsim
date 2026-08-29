#' Replications Needed to Reach a Target Monte Carlo Standard Error
#'
#' Monte Carlo standard errors shrink like `1 / sqrt(n_sim)`. Given the MCSE actually
#' achieved with `n_current` replications, this returns the number of replications a
#' future run would need to bring the MCSE down to `target`.
#'
#' @param mcse_current The Monte Carlo standard error observed so far. May be a vector.
#' @param n_current The number of replications that produced `mcse_current`. A single
#'   positive number, or a vector aligned with `mcse_current`.
#' @param target The desired Monte Carlo standard error. A single positive number, or a
#'   vector aligned with `mcse_current`.
#' @return An integer vector (`ceiling`) of the replications required to reach `target`.
#'   Entries are `NA` where `mcse_current` is `NA` or not positive.
#' @examples
#' # A coverage estimate with MCSE 0.011 from 1000 reps; how many reps for MCSE 0.005?
#' mcse_target(0.011, 1000, 0.005)
#'
#' # Vectorised over a summarise_sim() column
#' # mcse_target(tab$coverage_mcse, tab$n_sim, target = 0.005)
#' @export
mcse_target <- function(mcse_current, n_current, target) {
  if (!is.numeric(mcse_current)) stop("'mcse_current' must be numeric.")
  if (!is.numeric(n_current) || any(n_current <= 0, na.rm = TRUE)) {
    stop("'n_current' must be positive.")
  }
  if (!is.numeric(target) || length(target) == 0L || any(target <= 0, na.rm = TRUE)) {
    stop("'target' must be positive.")
  }

  needed <- n_current * (mcse_current / target)^2
  needed[!is.finite(mcse_current) | mcse_current <= 0] <- NA_real_
  out <- ceiling(needed)
  storage.mode(out) <- "integer"
  out
}


#' Flag Non-Finite or Degenerate Estimates in Simulation Output
#'
#' Scans the numeric columns of a replicate-level [runsim()] result for values that will
#' quietly distort performance measures: `NA`, `NaN`, and `Inf`. Reports the count and
#' proportion per condition, so a handful of blown-up replications do not pass unnoticed
#' into a summary table.
#'
#' @param data A dataframe of replicate-level results, typically from [runsim()].
#' @param cols Character vector of columns to check. Defaults to every numeric column
#'   except `ID` and the grouping columns in `by`.
#' @param by Character vector of column names identifying a simulation condition. If
#'   `NULL` (default) the whole dataframe is treated as one group.
#' @param digits Number of decimal places to round `prop_bad` to; `NULL` leaves it
#'   unrounded. Default 4.
#' @return A dataframe with one row per (condition, column) that has at least one
#'   non-finite value: the `by` columns, `column`, `n_bad`, `n_total`, and `prop_bad`,
#'   ordered by decreasing `prop_bad`. A zero-row dataframe (with those columns) means
#'   every checked value was finite.
#' @examples
#' sim_func <- function(mu) {
#'   x <- rnorm(10, mu)
#'   data.frame(est = mean(x), ratio = mu / (mu - 1))  # Inf at mu == 1
#' }
#' grid <- expand.grid(mu = c(0, 1, 2))
#' res <- runsim(20, grid, sim_func, show_progress = FALSE, num_cores = 1)
#' check_estimates(res, by = "mu")
#' @export
check_estimates <- function(data, cols = NULL, by = NULL, digits = 4) {
  if (!is.data.frame(data)) {
    stop("'data' must be a dataframe, typically the output of runsim().")
  }
  if (!is.null(by)) {
    if (!is.character(by)) stop("'by' must be a character vector of column names or NULL.")
    missing_cols <- setdiff(by, names(data))
    if (length(missing_cols)) {
      stop("column(s) not found in 'data': ", paste(missing_cols, collapse = ", "))
    }
  }

  if (is.null(cols)) {
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
    cols <- setdiff(numeric_cols, c("ID", by))
  } else {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols)) {
      stop("column(s) not found in 'data': ", paste(missing_cols, collapse = ", "))
    }
  }
  if (length(cols) == 0L) {
    stop("no columns to check; pass 'cols' explicitly.")
  }

  if (is.null(by)) {
    key_tab <- NULL
    group_rows <- list(seq_len(nrow(data)))
  } else {
    key_tab <- unique(data[, by, drop = FALSE])
    key_tab <- key_tab[do.call(order, as.list(key_tab)), , drop = FALSE]
    rownames(key_tab) <- NULL
    group_rows <- lapply(seq_len(nrow(key_tab)), function(i) {
      keep <- rep(TRUE, nrow(data))
      for (b in by) keep <- keep & data[[b]] == key_tab[[b]][i]
      which(keep)
    })
  }

  pieces <- list()
  for (gi in seq_along(group_rows)) {
    idx <- group_rows[[gi]]
    for (cl in cols) {
      v <- data[[cl]][idx]
      n_bad <- sum(!is.finite(v))
      if (n_bad == 0L) next
      stats <- data.frame(column = cl, n_bad = n_bad, n_total = length(v),
                          prop_bad = n_bad / length(v), stringsAsFactors = FALSE)
      row <- if (is.null(by)) stats else cbind(key_tab[gi, , drop = FALSE], stats)
      pieces[[length(pieces) + 1L]] <- row
    }
  }

  out_cols <- c(by, "column", "n_bad", "n_total", "prop_bad")
  if (length(pieces) == 0L) {
    empty <- data[0, by, drop = FALSE]
    empty$column <- character(0)
    empty$n_bad <- integer(0)
    empty$n_total <- integer(0)
    empty$prop_bad <- numeric(0)
    rownames(empty) <- NULL
    return(empty[, out_cols, drop = FALSE])
  }

  out <- do.call(rbind, pieces)
  out <- out[order(-out$prop_bad), out_cols, drop = FALSE]
  rownames(out) <- NULL
  if (!is.null(digits)) out$prop_bad <- .mcstatsim_round(out$prop_bad, digits)
  out
}
