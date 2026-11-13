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
