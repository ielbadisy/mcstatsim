library(testthat)

make_res <- function(n = 150) {
  sim_func <- function(mu, n_obs) {
    x <- rnorm(n_obs, mean = mu)
    est <- mean(x)
    se <- sd(x) / sqrt(n_obs)
    data.frame(est = est, lo = est - 1.96 * se, hi = est + 1.96 * se, true_val = mu)
  }
  grid <- expand.grid(mu = c(0, 2), n_obs = c(20, 40))
  runsim(n, grid, sim_func, show_progress = FALSE, num_cores = 1, seed = 1)
}

test_that("summarisesim produces one row per condition with measure + MCSE columns", {
  res <- make_res()
  s <- summarisesim(
    res,
    by = c("mu", "n_obs"),
    measures = list(
      bias     = ~ calcbias(est, true_val[1]),
      coverage = ~ calccoverage(lo, hi, true_val[1])
    )
  )
  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 4)
  expect_equal(names(s), c("mu", "n_obs", "n_sim", "bias", "bias_mcse", "coverage", "coverage_mcse"))
  expect_true(all(s$n_sim == 150))
  expect_true(all(abs(s$bias) < 0.1))
  expect_true(all(s$coverage > 0.8 & s$coverage <= 1))
})

test_that("summarisesim accepts function-valued measures", {
  res <- make_res()
  s <- summarisesim(
    res,
    by = "mu",
    measures = list(cov = function(d) calccoverage(d$lo, d$hi, d$true_val[1]))
  )
  expect_equal(nrow(s), 2)
  expect_true(all(c("coverage", "coverage_mcse") %in% names(s)))
})

test_that("summarisesim handles a scalar-valued measure and no grouping", {
  res <- make_res()
  s1 <- summarisesim(res, by = "n_obs", measures = list(mean_width = ~ mean(hi - lo)))
  expect_equal(names(s1), c("n_obs", "n_sim", "mean_width"))
  expect_equal(nrow(s1), 2)
  # narrower intervals with more observations
  expect_lt(s1$mean_width[s1$n_obs == 40], s1$mean_width[s1$n_obs == 20])

  s2 <- summarisesim(res, measures = list(k = ~ length(est)))
  expect_equal(nrow(s2), 1)
  expect_equal(s2$k, nrow(res))
})

test_that("summarisesim errors on unknown columns and bad measures", {
  res <- make_res()
  expect_error(summarisesim(res, by = "nope", measures = list(b = ~ mean(est))),
               "not found in 'data'")
  expect_error(summarisesim(res, by = "mu", measures = list()),
               "non-empty named list")
  expect_error(summarisesim(res, by = "mu", measures = list(~ mean(est))),
               "must be named")
  # passing a whole column as the true value -> non-scalar result
  expect_error(summarisesim(res, measures = list(b = ~ calcbias(est, true_val))),
               "non-scalar")
})

test_that("summarisesim prefixes colliding output columns with the measure name", {
  res <- make_res()
  out <- summarisesim(res, by = "mu",
                       measures = list(a = ~ c(bias = mean(est)), b = ~ c(bias = median(est))))
  expect_named(out, c("mu", "n_sim", "a_bias", "b_bias"))

  # two calc* measures returning the same names; a third, distinct measure is untouched
  out2 <- summarisesim(res, by = "mu",
                        measures = list(m1 = ~ calcbias(est, true_val[1]),
                                        m2 = ~ calcbias(lo, true_val[1]),
                                        cov = ~ calccoverage(lo, hi, true_val[1])))
  expect_true(all(c("m1_bias", "m1_bias_mcse", "m2_bias", "m2_bias_mcse") %in% names(out2)))
  expect_false(any(c("bias", "bias_mcse") %in% names(out2)))
  expect_true(all(names(calccoverage(0, 1, 0)) %in% names(out2)))
  g <- res[res$mu == res$mu[1], ]
  expect_equal(out2$m2_bias[1], calcbias(g$lo, g$true_val[1])$bias)

  # duplicated measure labels cannot be disambiguated
  expect_error(
    summarisesim(res, by = "mu",
                  measures = list(a = ~ c(bias = mean(est)), a = ~ c(bias = median(est)))),
    "collide"
  )
})

test_that("summarisesim rounds measure columns to digits (default 4), not the by/n_sim columns", {
  res <- make_res()
  meas <- list(mean_est = ~ mean(est))
  d4 <- summarisesim(res, by = "n_obs", measures = meas)          # default digits = 4
  d2 <- summarisesim(res, by = "n_obs", measures = meas, digits = 2)
  draw <- summarisesim(res, by = "n_obs", measures = meas, digits = NULL)

  expect_equal(d4$mean_est, round(draw$mean_est, 4))
  expect_equal(d2$mean_est, round(draw$mean_est, 2))
  expect_equal(d4$n_obs, draw$n_obs)      # grouping column untouched
  expect_equal(d4$n_sim, draw$n_sim)
  expect_error(summarisesim(res, by = "n_obs", measures = meas, digits = "x"),
               "single number or NULL")
})

test_that("summarisesim works when a column is named like an order() argument", {
  set.seed(1)
  res <- data.frame(p = rep(c(1e-4, 2e-4), each = 20),
                    method = rep(c("a", "b"), 20),
                    decreasing = rep(c("x", "y"), each = 10, times = 2),
                    lo = 0, hi = runif(40, 1e-5, 3e-5), ID = rep(1:10, 4))
  s <- summarisesim(res, by = c("p", "method", "decreasing"),
                    measures = list(w = ~ calcwidth(lo, hi)))
  expect_equal(nrow(s), 8)
  expect_identical(s$method, rep(c("a", "a", "b", "b"), 2))
  expect_identical(s$decreasing, rep(c("x", "y"), 4))
})

test_that("default digits keep tiny measures instead of rounding them to 0", {
  set.seed(2)
  res <- data.frame(lo = 0, hi = runif(50, 1e-5, 3e-5), est = rnorm(50, 0, 1e-6))
  s <- summarisesim(res, measures = list(w = ~ calcwidth(lo, hi),
                                         b = ~ calcbias(est, 0),
                                         m = function(d) mean(d$hi)))
  expect_true(s$width > 1e-5 && s$width < 3e-5)
  expect_equal(s$width, signif(mean(res$hi), 4))
  expect_equal(s$m, signif(mean(res$hi), 4))
  expect_true(s$bias != 0)
  expect_equal(calcwidth(0, 2e-5)$width, 2e-5)
  # values >= 1 are still rounded to decimal places
  expect_equal(mcstatsim:::.mcstatsim_round(123.456789, 4), 123.4568)
  expect_equal(mcstatsim:::.mcstatsim_round(c(0.123456, -0.0000123456, NA), 4),
               c(0.1235, -0.00001235, NA))
})
