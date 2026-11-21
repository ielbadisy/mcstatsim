# colour-blind-safe palette: the two shared accents first, then Okabe-Ito
PAL <- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#56B4E9", "#CC79A7", "#F0E442", "#000000")

# Internal: split `data` by the columns named in `by`, returning a named list of
# data frames (one per condition) in factor-level / numeric order. A NULL `by` yields
# a single group named "all".
.split_conditions <- function(data, by) {
  if (is.null(by)) return(list(all = data))
  key <- interaction(data[by], drop = TRUE, sep = " | ", lex.order = TRUE)
  split(data, key)
}

# display label of a column: `labels[v]` when given, else the column name
.lab <- function(v, labels) unname(ifelse(v %in% names(labels), labels[v], v))

# facet labeller giving "name = value" strips (one line); ".measure" shows the value only
.striplab <- function(labels) function(l) {
  out <- lapply(names(l), function(v) if (v == ".measure") as.character(l[[v]])
                else paste(.lab(v, labels), "=", l[[v]]))
  list(do.call(paste, c(out, sep = ", ")))
}

# stack per-condition frames, keeping the `by` columns (with their types and levels) and
# `condition` as a factor in panel order
.bind_conditions <- function(parts, by, f) {
  out <- do.call(rbind, lapply(names(parts), function(nm) {
    d <- parts[[nm]]
    if (nrow(d) == 0L) return(NULL)
    r <- f(d)
    r <- cbind(condition = nm, r, stringsAsFactors = FALSE)
    if (!is.null(by)) r <- cbind(d[rep(1L, nrow(r)), by, drop = FALSE], r)
    r
  }))
  out$condition <- factor(out$condition, levels = intersect(names(parts), out$condition))
  rownames(out) <- NULL
  out
}

.need_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function needs the 'ggplot2' package. Install it with ",
         "install.packages(\"ggplot2\").", call. = FALSE)
  }
}

# column names used inside aes() as bare symbols (data frames are built locally)
utils::globalVariables(c(
  "replication", "estimate", "lo", "hi", "centile", "covers", "true", "yintercept",
  ".x", ".value", ".lo", ".hi", ".colour", ".measure"
))


#' Convergence Trace of a Monte Carlo Estimate
#'
#' Plots the running mean of a quantity against the number of replications, with a
#' Monte Carlo standard error band, one panel per simulation condition. It shows whether
#' a run used enough replications for the quantity of interest to settle.
#'
#' For a 0/1 column (a running rejection rate or coverage) the default band is the
#' Wilson score interval with `z = se_mult`, which stays inside \[0, 1\]; other columns
#' get the Wald band `mean +/- se_mult * sd / sqrt(k)`.
#'
#' @param data A replicate-level dataframe, typically from [runsim()]. Replication order
#'   is taken from the `ID` column when present, otherwise from row order.
#' @param var Name of the numeric column to trace.
#' @param by Character vector of condition columns; one panel is drawn per combination,
#'   in the order of their factor levels (numeric order for numbers). Strips read
#'   `name = value`. `NULL` (default) traces the whole dataframe as one panel.
#' @param se_mult Half-width of the band in Monte Carlo standard errors (default 1).
#' @param band `"auto"` (default: Wilson for a 0/1 column, Wald otherwise), `"wald"` or
#'   `"wilson"`.
#' @param labels Optional named character vector of display names for columns
#'   (e.g. `c(n = "Sample size", rej = "Rejection rate")`), used in the axis and strips.
#' @param scales Facet scales passed to [ggplot2::facet_wrap()] (default `"free_y"`).
#' @param title,subtitle,caption Plot text; `NULL` (default) draws none.
#' @return A `ggplot` object. Its data holds the `by` columns, `condition` (a factor in
#'   panel order), `replication`, `estimate`, `lo` and `hi`.
#' @examples
#' \dontrun{
#' sim_func <- function(mu) data.frame(est = mean(rnorm(30, mu)))
#' res <- runsim(500, expand.grid(mu = c(0, 2)), sim_func,
#'               show_progress = FALSE, num_cores = 1, seed = 1)
#' plotconvergence(res, "est", by = "mu")
#' }
#' @export
plotconvergence <- function(data, var, by = NULL, se_mult = 1, band = c("auto", "wald", "wilson"),
                            labels = NULL, scales = "free_y", title = NULL, subtitle = NULL,
                            caption = NULL) {
  .need_ggplot2()
  band <- match.arg(band)
  if (!is.data.frame(data)) stop("'data' must be a dataframe.")
  if (!is.character(var) || length(var) != 1 || !var %in% names(data)) {
    stop("'var' must be the name of a single column in 'data'.")
  }
  if (!is.numeric(data[[var]])) stop("column '", var, "' is not numeric.")
  if (!is.null(by)) {
    miss <- setdiff(by, names(data))
    if (length(miss)) stop("column(s) not found in 'data': ", paste(miss, collapse = ", "))
  }
  if (band == "auto") band <- if (all(data[[var]] %in% c(0, 1))) "wilson" else "wald"

  traces <- .bind_conditions(.split_conditions(data, by), by, function(d) {
    if ("ID" %in% names(d)) d <- d[order(d$ID), , drop = FALSE]
    x <- d[[var]]; k <- seq_along(x); m <- cumsum(x) / k; z <- se_mult
    if (band == "wilson") {
      ctr <- (m + z^2 / (2 * k)) / (1 + z^2 / k)
      h <- z * sqrt(m * (1 - m) / k + z^2 / (4 * k^2)) / (1 + z^2 / k)
    } else {
      ctr <- m
      h <- z * sqrt((cumsum(x^2) - cumsum(x)^2 / k) / pmax(k - 1, 1) / k)
    }
    data.frame(replication = k, estimate = m, lo = ctr - h, hi = ctr + h)
  })

  p <- ggplot2::ggplot(traces, ggplot2::aes(x = replication, y = estimate)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "grey70", alpha = 0.5) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Replications", y = paste("Running mean of", .lab(var, labels)),
                  title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_bw()
  if (!is.null(by)) p <- p + ggplot2::facet_wrap(by, scales = scales, labeller = .striplab(labels))
  p
}


#' Zip Plot of Confidence-Interval Coverage
#'
#' Draws the zip plot of Morris, White and Crowther (2019): within each condition the
#' confidence intervals are sorted by how far their implied z-statistic sits from the
#' target value, stacked vertically by that fractional centile, and coloured by whether
#' they cover. A well-calibrated procedure fills the panel to the nominal line with the
#' covering colour and shows the non-covering intervals only in the top tail.
#'
#' @param data A replicate-level dataframe, typically from [runsim()].
#' @param lower,upper Column names of the interval bounds.
#' @param true Column name of the target value, or the target value itself (a number).
#' @param by Character vector of condition columns; one panel per combination, in the
#'   order of their factor levels (numeric order for numbers). Strips read
#'   `name = value`. The `by` columns are kept in the plot data, so
#'   `+ facet_wrap(~ method)` works.
#' @param conf_level Nominal confidence level the intervals were built at (default 0.95).
#' @param labels Optional named character vector of display names for the `by` columns
#'   (e.g. `c(n = "Sample size")`), used in the strips.
#' @param scales Facet scales passed to [ggplot2::facet_wrap()]. The default `"fixed"`
#'   puts all panels on one x axis so intervals can be compared across conditions.
#' @param colours Colours of the covering and non-covering intervals.
#' @param title,subtitle,caption Plot text; `NULL` (default) draws none.
#' @return A `ggplot` object. Its data holds the `by` columns, `condition` (a factor in
#'   panel order), `centile`, `lo`, `hi`, `true` and `covers`.
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
#' plotzip(res, "lo", "hi", "mu", by = "mu")
#' }
#' @export
plotzip <- function(data, lower, upper, true, by = NULL, conf_level = 0.95, labels = NULL,
                    scales = "fixed", colours = c(covers = "#0072B2", misses = "#D55E00"),
                    title = NULL, subtitle = NULL, caption = NULL) {
  .need_ggplot2()
  if (!is.data.frame(data)) stop("'data' must be a dataframe.")
  if (is.numeric(true)) { data$.true <- rep_len(true, nrow(data)); true <- ".true" }
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
  z <- .bind_conditions(.split_conditions(data, by), by, function(d) {
    lo <- d[[lower]]; hi <- d[[upper]]; tv <- d[[true]]
    est <- (lo + hi) / 2
    se <- (hi - lo) / (2 * crit)
    p_two <- 2 * stats::pnorm(-abs((est - tv) / se))
    covers <- lo <= tv & tv <= hi
    ord <- order(p_two, decreasing = TRUE)
    data.frame(
      centile = 100 * (seq_along(ord) - 0.5) / length(ord),
      lo = lo[ord], hi = hi[ord], true = tv[ord],
      covers = factor(ifelse(covers[ord], "covers", "misses"), c("covers", "misses"))
    )
  })

  p <- ggplot2::ggplot(z) +
    ggplot2::geom_segment(
      ggplot2::aes(x = lo, xend = hi, y = centile, yend = centile, colour = covers),
      linewidth = 0.3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = true), linetype = 2) +
    ggplot2::geom_hline(yintercept = 100 * conf_level, linetype = 3) +
    ggplot2::scale_colour_manual(values = colours, name = NULL, drop = FALSE) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = 2))) +
    ggplot2::labs(x = "Confidence interval", y = "Fractional centile of |z|",
                  title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_bw()
  if (!is.null(by)) p <- p + ggplot2::facet_wrap(by, scales = scales, labeller = .striplab(labels))
  p
}


#' Point-Range Plot of a Performance Measure Across Conditions
#'
#' Takes a summary table from [summarisesim()] and plots one or more performance
#' measures per condition as a point with a Monte Carlo error bar
#' (`estimate +/- se_mult * MCSE`), the standard "does the method work across the
#' design" picture.
#'
#' @param summary A dataframe from [summarisesim()].
#' @param measure Name(s) of the estimate column(s) to plot (e.g. `"coverage"`, or
#'   `c("bias", "coverage")` for one row of panels per measure). Each Monte Carlo
#'   standard error is taken from `paste0(measure, "_mcse")` when that column exists.
#' @param x Name of the column to place on the x axis (a design factor).
#' @param colour Optional column mapped to colour.
#' @param facet Optional column to facet by.
#' @param se_mult Half-height of the error bar in Monte Carlo standard errors (default 2).
#' @param ref Optional numeric reference line(s) (e.g. `0.95` for coverage, `0` for
#'   bias). With several measures, a named vector (`c(bias = 0, coverage = 0.95)`) or one
#'   value per measure draws each line in its own panel; otherwise all lines go in
#'   every panel.
#' @param labels Optional named character vector of display names for columns, e.g.
#'   `c(n = "Sample size", coverage = "Coverage", method = "Method")`, used in the axes,
#'   legend and strips (strips read `name = value`).
#' @param palette Optional colour values for `colour`. The default uses blue and
#'   vermillion, then the Okabe-Ito palette (up to 8 groups; ggplot2's default beyond).
#' @param scales Facet scales. `NULL` (default) means `"fixed"` for one measure and
#'   `"free_y"` for several.
#' @param title,subtitle,caption Plot text; `NULL` (default) draws none.
#' @return A `ggplot` object. Only `x`, `y` and `colour` are mapped globally (to `.x`,
#'   `.value` and `.colour`), so layers added with `+` need no `inherit.aes = FALSE`;
#'   the error bar uses `.lo` and `.hi`, and all columns of `summary` are kept.
#' @examples
#' \dontrun{
#' tab <- summarisesim(res, by = c("mu", "n_obs"),
#'                      measures = list(coverage = ~ calccoverage(lo, hi, true_val[1])))
#' plotperformance(tab, "coverage", x = "n_obs", colour = "mu", ref = 0.95)
#' }
#' @export
plotperformance <- function(summary, measure, x, colour = NULL, facet = NULL,
                            se_mult = 2, ref = NULL, labels = NULL, palette = NULL,
                            scales = NULL, title = NULL, subtitle = NULL, caption = NULL) {
  .need_ggplot2()
  if (!is.data.frame(summary)) stop("'summary' must be a dataframe from summarisesim().")
  need <- c(measure, x, colour, facet)
  miss <- setdiff(need, names(summary))
  if (length(miss)) stop("column(s) not found in 'summary': ", paste(miss, collapse = ", "))
  for (m in measure) if (!is.numeric(summary[[m]])) stop("column '", m, "' is not numeric.")

  ml <- .lab(measure, labels)
  d <- do.call(rbind, lapply(measure, function(m) {
    s <- summary; s$.value <- s[[m]]
    se <- s[[paste0(m, "_mcse")]]
    if (is.null(se)) se <- 0
    s$.lo <- s$.value - se_mult * se; s$.hi <- s$.value + se_mult * se
    s$.measure <- factor(.lab(m, labels), ml)
    s
  }))
  d$.x <- factor(d[[x]])
  multi <- length(measure) > 1
  if (is.null(scales)) scales <- if (multi) "free_y" else "fixed"

  aes_pt <- if (is.null(colour)) ggplot2::aes(x = .x, y = .value) else {
    d$.colour <- factor(d[[colour]])
    ggplot2::aes(x = .x, y = .value, colour = .colour, group = .colour)
  }

  p <- ggplot2::ggplot(d, aes_pt)
  if (!is.null(ref)) {
    nm <- if (!is.null(names(ref))) names(ref) else if (length(ref) == length(measure)) measure
    rd <- if (is.null(nm)) data.frame(yintercept = ref) else
      data.frame(.measure = factor(.lab(nm, labels), ml), yintercept = unname(ref))
    rd <- rd[stats::complete.cases(rd), , drop = FALSE]
    p <- p + ggplot2::geom_hline(data = rd, ggplot2::aes(yintercept = yintercept),
                                 linetype = 2, colour = "grey40", inherit.aes = FALSE)
  }
  p <- p +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = .lo, ymax = .hi),
                             position = ggplot2::position_dodge(width = 0.4)) +
    ggplot2::labs(x = .lab(x, labels), y = if (multi) NULL else ml,
                  colour = if (!is.null(colour)) .lab(colour, labels),
                  title = title, subtitle = subtitle, caption = caption) +
    ggplot2::theme_bw()
  if (!is.null(colour)) {
    k <- nlevels(d$.colour)
    pal <- if (!is.null(palette)) palette else if (k <= length(PAL)) PAL[seq_len(k)]
    if (!is.null(pal)) p <- p + ggplot2::scale_colour_manual(values = pal)
  }
  sl <- .striplab(labels)
  if (multi && !is.null(facet)) {
    p <- p + ggplot2::facet_grid(stats::as.formula(paste(".measure ~", facet)), scales = scales,
                                 labeller = sl, switch = "y")
  } else if (multi) {
    p <- p + ggplot2::facet_wrap(~ .measure, scales = scales, labeller = sl)
  } else if (!is.null(facet)) {
    p <- p + ggplot2::facet_wrap(facet, scales = scales, labeller = sl)
  }
  p
}
