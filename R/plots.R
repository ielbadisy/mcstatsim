# Internal: split `data` by the columns named in `by`, returning a named list of
# data frames (one per condition). A NULL `by` yields a single group named "all".
.split_conditions <- function(data, by) {
  if (is.null(by)) {
    grp <- list(data)
    names(grp) <- "all"
    return(grp)
  }
  key <- interaction(data[by], drop = TRUE, sep = " | ", lex.order = TRUE)
  split(data, key)
}

.need_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function needs the 'ggplot2' package. Install it with ",
         "install.packages(\"ggplot2\").", call. = FALSE)
  }
}

# column names used inside aes() as bare symbols (data frames are built locally)
utils::globalVariables(c(
  "replication", "estimate", "lo", "hi", "centile", "covers", "true",
  ".x", ".value", ".lo", ".hi", ".colour"
))


#' Convergence Trace of a Monte Carlo Estimate
#'
#' Plots the running mean of a quantity against the number of replications, with a
#' Monte Carlo standard error band, one panel per simulation condition. It shows whether
#' a run used enough replications for the quantity of interest to settle.
#'
#' @param data A replicate-level dataframe, typically from [runsim()]. Replication order
#'   is taken from the `ID` column when present, otherwise from row order.
#' @param var Name of the numeric column to trace.
#' @param by Character vector of condition columns; one panel is drawn per combination.
#'   `NULL` (default) traces the whole dataframe as one panel.
#' @param se_mult Half-width of the band in Monte Carlo standard errors (default 1).
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' sim_func <- function(mu) data.frame(est = mean(rnorm(30, mu)))
#' res <- runsim(500, expand.grid(mu = c(0, 2)), sim_func,
#'               show_progress = FALSE, num_cores = 1, seed = 1)
#' plot_convergence(res, "est", by = "mu")
#' }
#' @export
plot_convergence <- function(data, var, by = NULL, se_mult = 1) {
  .need_ggplot2()
  if (!is.data.frame(data)) stop("'data' must be a dataframe.")
  if (!is.character(var) || length(var) != 1 || !var %in% names(data)) {
    stop("'var' must be the name of a single column in 'data'.")
  }
  if (!is.numeric(data[[var]])) stop("column '", var, "' is not numeric.")
  if (!is.null(by)) {
    miss <- setdiff(by, names(data))
    if (length(miss)) stop("column(s) not found in 'data': ", paste(miss, collapse = ", "))
  }

  parts <- .split_conditions(data, by)
  traces <- do.call(rbind, lapply(names(parts), function(nm) {
    d <- parts[[nm]]
    if (nrow(d) == 0L) return(NULL)
    if ("ID" %in% names(d)) d <- d[order(d$ID), , drop = FALSE]
    x <- d[[var]]
    k <- seq_along(x)
    run_mean <- cumsum(x) / k
    run_var <- (cumsum(x^2) - cumsum(x)^2 / k) / pmax(k - 1, 1)
    run_se <- sqrt(run_var / k)
    data.frame(condition = nm, replication = k, estimate = run_mean,
               lo = run_mean - se_mult * run_se, hi = run_mean + se_mult * run_se,
               stringsAsFactors = FALSE)
  }))

  p <- ggplot2::ggplot(traces, ggplot2::aes(x = replication, y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "replication", y = paste0("running mean of ", var),
                  title = paste0("Convergence trace (", u_signif(se_mult), " MCSE band)")) +
    ggplot2::theme_bw()
  if (!is.null(by)) p <- p + ggplot2::facet_wrap(~ condition, scales = "free_y")
  p
}

# small helper: format se_mult without trailing zeros for the title
u_signif <- function(x) format(x, trim = TRUE, drop0trailing = TRUE)


#' Zip Plot of Confidence-Interval Coverage
#'
#' Draws the zip plot of Morris, White and Crowther (2019): within each condition the
#' confidence intervals are sorted by how far their implied z-statistic sits from the
#' target value, stacked vertically by that fractional centile, and coloured by whether
#' they cover. A well-calibrated procedure fills the panel to the nominal line with the
#' covering colour and shows the non-covering intervals only in the top tail.
#'
#' @param data A replicate-level dataframe, typically from [runsim()].
#' @param lower,upper,true Column names of the interval bounds and the target value.
#' @param by Character vector of condition columns; one panel per combination.
#' @param conf_level Nominal confidence level the intervals were built at (default 0.95).
#' @return A `ggplot` object.
#' @references Morris TP, White IR, Crowther MJ (2019). Using simulation studies to
#'   evaluate statistical methods. *Statistics in Medicine*, 38(11), 2074-2102.
#' @examples
#' \dontrun{
#' sim_func <- function(mu) {
#'   x <- rnorm(25, mu); est <- mean(x); se <- sd(x) / 5
#'   data.frame(lo = est - 1.96 * se, hi = est + 1.96 * se, mu = mu)
#' }
#' res <- runsim(300, expand.grid(mu = c(0, 1)), sim_func,
#'               show_progress = FALSE, num_cores = 1, seed = 1)
#' plot_zip(res, "lo", "hi", "mu", by = "mu")
#' }
#' @export
plot_zip <- function(data, lower, upper, true, by = NULL, conf_level = 0.95) {
  .need_ggplot2()
  if (!is.data.frame(data)) stop("'data' must be a dataframe.")
  for (nm in c(lower, upper, true)) {
    if (!is.character(nm) || length(nm) != 1 || !nm %in% names(data)) {
      stop("'lower', 'upper' and 'true' must each name a single column in 'data'.")
    }
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1 || conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be a single number in (0, 1).")
  }
  if (!is.null(by)) {
    miss <- setdiff(by, names(data))
    if (length(miss)) stop("column(s) not found in 'data': ", paste(miss, collapse = ", "))
  }

  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  parts <- .split_conditions(data, by)
  z <- do.call(rbind, lapply(names(parts), function(nm) {
    d <- parts[[nm]]
    if (nrow(d) == 0L) return(NULL)
    lo <- d[[lower]]; hi <- d[[upper]]; tv <- d[[true]]
    est <- (lo + hi) / 2
    se <- (hi - lo) / (2 * crit)
    p_two <- 2 * stats::pnorm(-abs((est - tv) / se))
    covers <- lo <= tv & tv <= hi
    ord <- order(p_two, decreasing = TRUE)
    data.frame(
      condition = nm,
      centile = 100 * (seq_along(ord) - 0.5) / length(ord),
      lo = lo[ord], hi = hi[ord], true = tv[ord],
      covers = ifelse(covers[ord], "covers", "misses"),
      stringsAsFactors = FALSE
    )
  }))

  p <- ggplot2::ggplot(z) +
    ggplot2::geom_segment(
      ggplot2::aes(x = lo, xend = hi, y = centile, yend = centile, colour = covers),
      linewidth = 0.3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = true), linetype = 2) +
    ggplot2::geom_hline(yintercept = 100 * conf_level, linetype = 3) +
    ggplot2::scale_colour_manual(values = c(covers = "#4477AA", misses = "#EE6677"), name = NULL) +
    ggplot2::labs(x = "confidence interval", y = "fractional centile of |z|",
                  title = paste0("Zip plot (", 100 * conf_level, "% intervals)")) +
    ggplot2::theme_bw()
  if (!is.null(by)) p <- p + ggplot2::facet_wrap(~ condition, scales = "free_x")
  p
}


#' Point-Range Plot of a Performance Measure Across Conditions
#'
#' Takes a summary table from [summarise_sim()] and plots one performance measure per
#' condition as a point with a Monte Carlo error bar (`estimate +/- se_mult * MCSE`),
#' the standard "does the method work across the design" picture.
#'
#' @param summary A dataframe from [summarise_sim()].
#' @param measure Name of the estimate column to plot (e.g. `"coverage"`). Its Monte
#'   Carlo standard error is taken from `paste0(measure, "_mcse")` when that column
#'   exists.
#' @param x Name of the column to place on the x axis (a design factor).
#' @param colour Optional column mapped to colour.
#' @param facet Optional column to facet by.
#' @param se_mult Half-height of the error bar in Monte Carlo standard errors (default 2).
#' @param ref Optional numeric reference line (e.g. `0.95` for coverage, `0` for bias).
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' tab <- summarise_sim(res, by = c("mu", "n_obs"),
#'                      measures = list(coverage = ~ calc_coverage(lo, hi, true_val[1])))
#' plot_performance(tab, "coverage", x = "n_obs", colour = "mu", ref = 0.95)
#' }
#' @export
plot_performance <- function(summary, measure, x, colour = NULL, facet = NULL,
                             se_mult = 2, ref = NULL) {
  .need_ggplot2()
  if (!is.data.frame(summary)) stop("'summary' must be a dataframe from summarise_sim().")
  need <- c(measure, x, colour, facet)
  miss <- setdiff(need, names(summary))
  if (length(miss)) stop("column(s) not found in 'summary': ", paste(miss, collapse = ", "))
  if (!is.numeric(summary[[measure]])) stop("column '", measure, "' is not numeric.")

  d <- summary
  d$.value <- d[[measure]]
  mcse_col <- paste0(measure, "_mcse")
  if (mcse_col %in% names(d)) {
    d$.lo <- d$.value - se_mult * d[[mcse_col]]
    d$.hi <- d$.value + se_mult * d[[mcse_col]]
  } else {
    d$.lo <- d$.value
    d$.hi <- d$.value
  }
  d$.x <- factor(d[[x]])

  aes_pt <- if (is.null(colour)) {
    ggplot2::aes(x = .x, y = .value, ymin = .lo, ymax = .hi)
  } else {
    d$.colour <- factor(d[[colour]])
    ggplot2::aes(x = .x, y = .value, ymin = .lo, ymax = .hi, colour = .colour, group = .colour)
  }

  p <- ggplot2::ggplot(d, aes_pt)
  if (!is.null(ref)) p <- p + ggplot2::geom_hline(yintercept = ref, linetype = 2)
  p <- p +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.4)) +
    ggplot2::labs(x = x, y = measure, colour = colour,
                  title = paste0(measure, " by ", x,
                                 if (mcse_col %in% names(summary)) paste0(" (+/- ", u_signif(se_mult), " MCSE)") else "")) +
    ggplot2::theme_bw()
  if (!is.null(facet)) p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet)))
  p
}
