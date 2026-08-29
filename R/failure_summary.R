#' Summarise Failed Jobs from a Simulation Run
#'
#' Aggregates the per-job failure log attached to a [runsim()] result (when it was
#' called with `on_error = "warn"` or `on_error = "omit"`) into one row per
#' simulation condition, with the number of failed replications, the failure rate,
#' and an example error message.
#'
#' @param x A dataframe returned by [runsim()].
#' @param n Optional total number of replications per condition, used to turn failure
#'   counts into rates. If `NULL` (default) it is taken as `max(x$ID)`.
#' @param digits Number of decimal places to round `failure_rate` to; `NULL` leaves it
#'   unrounded. Default 4.
#' @return A dataframe with the grid columns that describe each condition, plus
#'   `n_failed`, `failure_rate`, and `example_error`. If `x` has no `"errors"`
#'   attribute (no job failed, or `on_error = "stop"` was used) a zero-row dataframe
#'   with those columns is returned.
#' @examples
#' sim_func <- function(mu) {
#'   if (mu < 0) stop("mu must be non-negative")
#'   data.frame(est = mean(rnorm(10, mu)))
#' }
#' params <- expand.grid(mu = c(-1, 0, 1))
#' res <- runsim(5, params, sim_func, show_progress = FALSE, num_cores = 1,
#'               on_error = "omit")
#' failure_summary(res)
#' @export
failure_summary <- function(x, n = NULL, digits = 4) {
  errors <- attr(x, "errors")
  cond_cols <- setdiff(names(errors), c("ID", "error"))

  if (is.null(errors) || nrow(errors) == 0L) {
    empty <- if (is.null(errors)) {
      data.frame()
    } else {
      errors[0, cond_cols, drop = FALSE]
    }
    empty$n_failed <- integer(0)
    empty$failure_rate <- numeric(0)
    empty$example_error <- character(0)
    return(empty)
  }

  if (is.null(n)) {
    n <- if (!is.null(x$ID)) max(x$ID) else max(errors$ID)
  }

  key <- if (length(cond_cols) == 0L) {
    rep("all", nrow(errors))
  } else {
    do.call(paste, c(errors[cond_cols], sep = "\r"))
  }
  parts <- split(seq_len(nrow(errors)), key)

  rows <- lapply(parts, function(idx) {
    base <- if (length(cond_cols) == 0L) {
      data.frame(row.names = 1L)
    } else {
      errors[idx[1], cond_cols, drop = FALSE]
    }
    base$n_failed <- length(idx)
    base$failure_rate <- length(idx) / n
    base$example_error <- errors$error[idx[1]]
    base
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  if (!is.null(digits)) out$failure_rate <- .mcstatsim_round(out$failure_rate, digits)
  out[order(-out$n_failed), , drop = FALSE]
}
