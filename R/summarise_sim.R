#' Summarise Replicate-Level Simulation Output into a Performance Table
#'
#' Collapses the raw, replicate-level dataframe returned by [runsim()] into one row per
#' simulation condition, evaluating one or more performance measures (bias, coverage,
#' rejection rate, ...) per condition. This is the bridge between `runsim()` output and
#' the `calc_*` functions in this package.
#'
#' @param data A dataframe of replicate-level results, typically from [runsim()]: one row
#'   per replication (per condition), with the condition columns, the quantities returned
#'   by `sim_func`, and usually an `ID` column.
#' @param by Character vector of column names identifying a simulation condition. Rows
#'   sharing the same values in these columns form one group. If `NULL` (default) the
#'   whole dataframe is treated as a single group.
#' @param measures A named list of performance measures to evaluate per group. Each element
#'   is either
#'   \itemize{
#'     \item a one-sided formula whose right-hand side is evaluated with the group's
#'       columns in scope, e.g. `~ calc_bias(est, true_val[1])`; or
#'     \item a function taking the group dataframe and returning a named numeric vector or
#'       a list of scalars, e.g. `function(d) calc_coverage(d$lo, d$hi, d$true_val[1])`.
#'   }
#'   Columns hold one value per replicate, so a quantity that is constant across a
#'   condition (a true parameter value) must be indexed, e.g. `true_val[1]`.
#'   The element names are labels only. A measure that returns a length-one value becomes a
#'   column named after the measure; a measure that returns a named vector of length > 1
#'   (as the `calc_*` functions do) contributes one column per name. Column-name collisions
#'   across measures are an error.
#' @return A dataframe with one row per group: the `by` columns, then `n_sim` (the number of
#'   distinct `ID` values in the group, or the row count if there is no `ID` column), then
#'   one column per performance-measure output.
#' @examples
#' sim_func <- function(mu, n_obs) {
#'   x <- rnorm(n_obs, mean = mu)
#'   est <- mean(x)
#'   se <- sd(x) / sqrt(n_obs)
#'   data.frame(est = est, lo = est - 1.96 * se, hi = est + 1.96 * se, true_val = mu)
#' }
#' grid <- expand.grid(mu = c(0, 1), n_obs = c(20, 50))
#' res <- runsim(100, grid, sim_func, show_progress = FALSE, num_cores = 1, seed = 1)
#'
#' summarise_sim(
#'   res,
#'   by = c("mu", "n_obs"),
#'   measures = list(
#'     bias     = ~ calc_bias(est, true_val[1]),
#'     coverage = ~ calc_coverage(lo, hi, true_val[1])
#'   )
#' )
#' @export
summarise_sim <- function(data, by = NULL, measures) {
  if (!is.data.frame(data)) {
    stop("'data' must be a dataframe, typically the output of runsim().")
  }
  if (missing(measures) || !is.list(measures) || length(measures) == 0L) {
    stop("'measures' must be a non-empty named list.")
  }
  if (is.null(names(measures)) || any(!nzchar(names(measures)))) {
    stop("every element of 'measures' must be named.")
  }
  if (!is.null(by)) {
    if (!is.character(by)) stop("'by' must be a character vector of column names or NULL.")
    missing_cols <- setdiff(by, names(data))
    if (length(missing_cols)) {
      stop("column(s) not found in 'data': ", paste(missing_cols, collapse = ", "))
    }
  }

  eval_one <- function(m, lab, gdf) {
    if (inherits(m, "formula")) {
      out <- eval(m[[length(m)]], envir = gdf, enclos = environment(m))
    } else if (is.function(m)) {
      out <- m(gdf)
    } else {
      stop("each element of 'measures' must be a one-sided formula or a function.")
    }
    if (is.list(out) && any(lengths(out) != 1L)) {
      stop("measure '", lab, "' returned non-scalar element(s); each performance ",
           "measure must reduce a condition's replicates to scalar(s). ",
           "If you passed a constant column as the true value, index it, ",
           "e.g. calc_bias(est, true_val[1]).")
    }
    out <- unlist(out, use.names = TRUE)
    if (!is.numeric(out) || length(out) == 0L) {
      stop("measure '", lab, "' returned a ", class(out)[1], " of length ", length(out),
           "; measures must return numeric scalars.")
    }
    out
  }

  # group row indices, and the key table describing each group
  if (is.null(by)) {
    group_rows <- list(seq_len(nrow(data)))
    key_tab <- NULL
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

  summarise_group <- function(gdf) {
    vals <- Map(function(m, lab) eval_one(m, lab, gdf), measures, names(measures))
    named <- Map(function(v, lab) {
      if (is.null(names(v))) {
        names(v) <- if (length(v) == 1L) lab else paste0(lab, seq_along(v))
      }
      v
    }, vals, names(measures))
    do.call(c, unname(named))
  }

  flats <- lapply(group_rows, function(idx) summarise_group(data[idx, , drop = FALSE]))

  col_names <- names(flats[[1]])
  if (anyDuplicated(col_names)) {
    stop("performance-measure output columns collide: ",
         paste(unique(col_names[duplicated(col_names)]), collapse = ", "),
         ". Rename the returned elements so every column is unique.")
  }

  n_sim <- vapply(group_rows, function(idx) {
    if ("ID" %in% names(data)) length(unique(data$ID[idx])) else length(idx)
  }, numeric(1))

  measure_mat <- do.call(rbind, lapply(flats, function(v) v[col_names]))
  out <- data.frame(measure_mat, stringsAsFactors = FALSE)
  names(out) <- col_names
  out <- cbind(n_sim = n_sim, out)
  if (!is.null(by)) out <- cbind(key_tab, out)
  rownames(out) <- NULL
  out
}
