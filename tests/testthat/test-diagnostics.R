library(testthat)

test_that("mcse_target scales replications with the inverse-square of the MCSE ratio", {
  expect_equal(mcse_target(0.01, 1000, 0.005), 4000L)
  expect_equal(mcse_target(0.02, 500, 0.02), 500L)
  # halving the target quadruples the reps
  expect_equal(mcse_target(0.1, 100, 0.05), 400L)
})

test_that("mcse_target is vectorised and returns NA for non-positive/NA MCSE", {
  res <- mcse_target(c(0.01, NA, 0), c(1000, 1000, 1000), target = 0.005)
  expect_equal(res, c(4000L, NA, NA))
  expect_type(res, "integer")
})

test_that("mcse_target validates its arguments", {
  expect_error(mcse_target(0.01, -1, 0.005), "'n_current' must be positive")
  expect_error(mcse_target(0.01, 1000, 0), "'target' must be positive")
  expect_error(mcse_target("x", 1000, 0.005), "must be numeric")
})

test_that("check_estimates finds non-finite values per condition", {
  sim_func <- function(mu) data.frame(est = mean(rnorm(10, mu)), ratio = mu / (mu - 1))
  grid <- expand.grid(mu = c(0, 1, 2))
  res <- runsim(20, grid, sim_func, show_progress = FALSE, num_cores = 1)

  chk <- check_estimates(res, by = "mu")
  expect_s3_class(chk, "data.frame")
  expect_equal(names(chk), c("mu", "column", "n_bad", "n_total", "prop_bad"))
  expect_equal(nrow(chk), 1)
  expect_equal(chk$mu, 1)
  expect_equal(chk$column, "ratio")
  expect_equal(chk$n_bad, 20)
  expect_equal(chk$prop_bad, 1)
})

test_that("check_estimates returns an empty frame when all values are finite", {
  sim_func <- function(mu) data.frame(est = mean(rnorm(10, mu)))
  grid <- expand.grid(mu = c(0, 1))
  res <- runsim(15, grid, sim_func, show_progress = FALSE, num_cores = 1)

  chk <- check_estimates(res, by = "mu")
  expect_equal(nrow(chk), 0)
  expect_equal(names(chk), c("mu", "column", "n_bad", "n_total", "prop_bad"))
})

test_that("check_estimates works without grouping and with an explicit cols argument", {
  df <- data.frame(a = c(1, Inf, 3, NaN), b = c(1, 2, 3, 4), ID = 1:4)
  chk <- check_estimates(df, cols = "a")
  expect_equal(chk$column, "a")
  expect_equal(chk$n_bad, 2)
  expect_equal(chk$n_total, 4)

  # default cols excludes ID
  chk2 <- check_estimates(df)
  expect_setequal(chk2$column, "a")
})

test_that("check_estimates rounds prop_bad to digits (default 4)", {
  df <- data.frame(a = c(rep(1, 6), Inf), ID = 1:7)   # prop_bad = 1/7 = 0.142857...
  expect_equal(check_estimates(df, cols = "a")$prop_bad, round(1 / 7, 4))
  expect_equal(check_estimates(df, cols = "a", digits = 2)$prop_bad, 0.14)
  expect_equal(check_estimates(df, cols = "a", digits = NULL)$prop_bad, 1 / 7)
})
