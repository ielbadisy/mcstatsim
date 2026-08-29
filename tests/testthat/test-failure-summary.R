library(testthat)

test_that("failure_summary aggregates failed jobs per condition", {
  sim_func <- function(mu) {
    if (mu < 0) stop("mu must be non-negative")
    data.frame(est = mean(rnorm(10, mu)))
  }
  params <- expand.grid(mu = c(-1, 0, 2))
  res <- runsim(5, params, sim_func, show_progress = FALSE, num_cores = 1, on_error = "omit")

  fs <- failure_summary(res)
  expect_s3_class(fs, "data.frame")
  expect_equal(nrow(fs), 1)
  expect_equal(fs$mu, -1)
  expect_equal(fs$n_failed, 5)
  expect_equal(fs$failure_rate, 1)
  expect_true(grepl("non-negative", fs$example_error))
})

test_that("failure_summary returns an empty frame when nothing failed", {
  sim_func <- function(mu) data.frame(est = mu)
  params <- expand.grid(mu = 1:3)
  res <- runsim(4, params, sim_func, show_progress = FALSE, num_cores = 1, on_error = "warn")

  fs <- failure_summary(res)
  expect_s3_class(fs, "data.frame")
  expect_equal(nrow(fs), 0)
  expect_true(all(c("n_failed", "failure_rate", "example_error") %in% names(fs)))
})

test_that("failure_summary honours an explicit n and orders by descending n_failed", {
  sim_func <- function(k) {
    if (k == 2) stop("k2 always fails")
    if (k == 1 && rnorm(1) > 0) stop("k1 sometimes fails")
    data.frame(x = k)
  }
  params <- expand.grid(k = 1:3)
  res <- runsim(8, params, sim_func, show_progress = FALSE, num_cores = 1,
                seed = 1, on_error = "omit")

  fs <- failure_summary(res, n = 8)
  expect_equal(fs$k[1], 2)
  expect_equal(fs$n_failed[1], 8)
  expect_equal(fs$failure_rate[1], 1)
  expect_true(all(diff(fs$n_failed) <= 0))
  expect_true(fs$n_failed[2] > 0 && fs$n_failed[2] < 8)
})

test_that("failure_summary rounds failure_rate to digits (default 4)", {
  sim_func <- function(k) if (k == 1 && rnorm(1) > 0) stop("x") else data.frame(v = k)
  res <- runsim(7, expand.grid(k = 1:2), sim_func, show_progress = FALSE,
                num_cores = 1, seed = 3, on_error = "omit")
  fr <- failure_summary(res)$failure_rate
  expect_equal(fr, round(fr, 4))
  fr_raw <- failure_summary(res, digits = NULL)$failure_rate
  expect_equal(failure_summary(res, digits = 2)$failure_rate, round(fr_raw, 2))
})
