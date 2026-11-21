library(testthat)

skip_if_no_ggplot2 <- function() {
  testthat::skip_if_not_installed("ggplot2")
}

make_res <- function(n = 120) {
  sim_func <- function(mu, n_obs) {
    x <- rnorm(n_obs, mu)
    est <- mean(x)
    se <- sd(x) / sqrt(n_obs)
    data.frame(est = est, lo = est - 1.96 * se, hi = est + 1.96 * se, true_val = mu)
  }
  grid <- expand.grid(mu = c(0, 2), n_obs = c(15, 40))
  runsim(n, grid, sim_func, show_progress = FALSE, num_cores = 1, seed = 1)
}

test_that("plotconvergence returns a ggplot with running mean and MCSE band", {
  skip_if_no_ggplot2()
  res <- make_res()
  p <- plotconvergence(res, "est", by = c("mu", "n_obs"))
  expect_s3_class(p, "ggplot")
  d <- p$data
  expect_true(all(c("condition", "replication", "estimate", "lo", "hi") %in% names(d)))
  # one trace point per replication per condition
  expect_equal(nrow(d), nrow(res))
  # running mean ends at the overall mean within a condition
  last <- d[d$condition == d$condition[1], ]
  sub <- res[res$mu == res$mu[1] & res$n_obs == res$n_obs[1], ]
  expect_equal(tail(last$estimate, 1), mean(sub$est))
})

test_that("plotconvergence validates its inputs", {
  skip_if_no_ggplot2()
  res <- make_res()
  expect_error(plotconvergence(res, "nope"), "single column")
  expect_error(plotconvergence(res, "est", by = "missing"), "not found")
})

test_that("plotzip returns a ggplot and classifies coverage", {
  skip_if_no_ggplot2()
  res <- make_res()
  p <- plotzip(res, "lo", "hi", "true_val", by = "n_obs")
  expect_s3_class(p, "ggplot")
  d <- p$data
  expect_true(all(c("centile", "lo", "hi", "covers") %in% names(d)))
  expect_setequal(unique(d$covers), c("covers", "misses"))
  expect_true(all(d$centile >= 0 & d$centile <= 100))
})

test_that("plotzip validates conf_level and columns", {
  skip_if_no_ggplot2()
  res <- make_res()
  expect_error(plotzip(res, "lo", "hi", "true_val", conf_level = 1.2), "in \\(0, 1\\)")
  expect_error(plotzip(res, "lo", "hi", "absent"), "name a single column")
})

test_that("plotperformance draws a point-range from a summarisesim table", {
  skip_if_no_ggplot2()
  res <- make_res()
  tab <- summarisesim(
    res, by = c("mu", "n_obs"),
    measures = list(coverage = ~ calccoverage(lo, hi, true_val[1]))
  )
  p <- plotperformance(tab, "coverage", x = "n_obs", colour = "mu", ref = 0.95)
  expect_s3_class(p, "ggplot")
  expect_true(".lo" %in% names(p$data) && ".hi" %in% names(p$data))
  expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomPointrange"), logical(1))))
})

test_that("plotperformance errors on missing columns", {
  skip_if_no_ggplot2()
  res <- make_res()
  tab <- summarisesim(res, by = "n_obs",
                       measures = list(bias = ~ calcbias(est, true_val[1])))
  expect_error(plotperformance(tab, "coverage", x = "n_obs"), "not found in 'summary'")
})

zipres <- function() {
  sf <- function(n, method) {
    x <- rnorm(n); s <- sd(x) / sqrt(n)
    data.frame(est = mean(x), lo = mean(x) - 1.96 * s, hi = mean(x) + 1.96 * s, true = 0,
               p = 2 * pnorm(-abs(mean(x) / s)))
  }
  g <- expand.grid(n = c(10, 25, 55, 200), method = factor(c("wald", "bca"), c("wald", "bca")))
  runsim(60, g, sf, show_progress = FALSE, num_cores = 1, seed = 2)
}
strips <- function(p) {
  b <- ggplot2::ggplot_build(p)
  unlist(b$layout$facet$params$labeller(b$layout$layout[names(b$layout$facet$params$facets)]))
}

test_that("plotzip orders panels by numeric value and factor levels, strips name = value", {
  skip_if_no_ggplot2()
  res <- zipres()
  p <- plotzip(res, "lo", "hi", "true", by = "n")
  expect_equal(levels(p$data$condition), c("10", "25", "55", "200"))
  expect_equal(strips(p), paste("n =", c(10, 25, 55, 200)))
  p2 <- plotzip(res, "lo", "hi", "true", by = c("method", "n"), labels = c(method = "Interval"))
  expect_equal(strips(p2)[1:2], c("Interval = wald, n = 10", "Interval = wald, n = 25"))
  expect_true(is.factor(p2$data$method) && is.numeric(p2$data$n))
  expect_s3_class(ggplot2::ggplot_build(p2 + ggplot2::facet_wrap(~ method)), "ggplot_built")
})

test_that("plotzip takes a numeric true, no default title, fixed scales, thick legend keys", {
  skip_if_no_ggplot2()
  res <- zipres()
  p <- plotzip(res, "lo", "hi", 0, by = "n")
  expect_equal(p$data, plotzip(res, "lo", "hi", "true", by = "n")$data)
  expect_null(p$labels$title)
  expect_false(p$facet$params$free$x)
  expect_equal(p$guides$guides$colour$params$override.aes$linewidth, 2)
  expect_equal(plotzip(res, "lo", "hi", 0, title = "Zip")$labels$title, "Zip")
  expect_true(plotzip(res, "lo", "hi", 0, by = "n", scales = "free_x")$facet$params$free$x)
})

test_that("plotconvergence uses a Wilson band for 0/1 data that stays in [0, 1]", {
  skip_if_no_ggplot2()
  res <- zipres()
  res$rej <- as.numeric(res$p < 0.05)
  p <- plotconvergence(res, "rej", by = "n", se_mult = 2)
  d <- p$data
  expect_true(all(d$lo >= 0 & d$hi <= 1))
  expect_equal(levels(d$condition), c("10", "25", "55", "200"))
  expect_null(p$labels$title)
  # Wilson bounds match prop.test(correct = FALSE) at z = 2
  r <- d[d$n == 10, ][30, ]
  x <- res[res$n == 10, ]; x <- x[order(x$ID), "rej"][1:30]
  ci <- prop.test(sum(x), 30, conf.level = 2 * pnorm(2) - 1, correct = FALSE)$conf.int
  expect_equal(c(r$lo, r$hi), as.numeric(ci), tolerance = 1e-10)
  # Wald band unchanged when asked for
  w <- plotconvergence(res, "rej", by = "n", se_mult = 2, band = "wald")$data
  expect_true(any(w$lo < 0))
  expect_equal(w$estimate, d$estimate)
})

test_that("plotperformance: labels, palette, strips, no title, several measures and refs", {
  skip_if_no_ggplot2()
  res <- zipres()
  tab <- summarisesim(res, by = c("n", "method"),
    measures = list(coverage = ~ calccoverage(lo, hi, true[1]), bias = ~ calcbias(est, true[1])))
  p <- plotperformance(tab, "coverage", x = "n", colour = "method", facet = "n", ref = 0.95,
                       labels = c(n = "Sample size", coverage = "Coverage", method = "Interval"))
  expect_null(p$labels$title)
  expect_equal(c(p$labels$x, p$labels$y, p$labels$colour), c("Sample size", "Coverage", "Interval"))
  b <- ggplot2::ggplot_build(p)
  expect_setequal(unique(b$data[[2]]$colour), c("#0072B2", "#D55E00"))
  expect_equal(strips(p)[1], "Sample size = 10")
  expect_equal(plotperformance(tab, "coverage", "n", colour = "method",
                               palette = c("black", "grey50"))$scales$scales[[1]]$palette(2),
               c("black", "grey50"))
  # added layers need no inherit.aes = FALSE
  expect_s3_class(ggplot2::ggplot_build(p + ggplot2::geom_line()), "ggplot_built")
  q <- plotperformance(tab, c("bias", "coverage"), x = "n", colour = "method",
                       ref = c(bias = 0, coverage = 0.95))
  bq <- ggplot2::ggplot_build(q)
  expect_equal(levels(q$data$.measure), c("bias", "coverage"))
  expect_true(q$facet$params$free$y)
  h <- bq$data[[1]]
  expect_equal(h$yintercept[order(h$PANEL)], c(0, 0.95))
  expect_equal(nrow(q$data), 2 * nrow(tab))
  expect_s3_class(ggplot2::ggplot_build(
    plotperformance(tab, c("bias", "coverage"), x = "method", facet = "n", ref = c(0, 0.95))),
    "ggplot_built")
})
