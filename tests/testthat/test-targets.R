library(testthat)

test_that("calc_relative_rmse is the sqrt of calc_relative_mse", {
  set.seed(1)
  est <- rnorm(200, mean = 5, sd = 2)
  tp <- 5

  rm <- calc_relative_mse(est, tp)
  rr <- calc_relative_rmse(est, tp)

  expect_equal(rr$rel_rmse, sqrt(rm$rel_mse))
})

test_that("calc_relative_rmse MCSE follows the delta method, not sqrt() of the MSE MCSE", {
  set.seed(2)
  est <- rnorm(200, mean = 10, sd = 3)
  tp <- 10

  rm <- calc_relative_mse(est, tp)
  rr <- calc_relative_rmse(est, tp)

  expect_equal(rr$rel_rmse_mcse, rm$rel_mse_mcse / (2 * sqrt(rm$rel_mse)))
  # the old implementation returned sqrt(rel_mse_mcse); make sure we no longer do
  expect_false(isTRUE(all.equal(rr$rel_rmse_mcse, sqrt(rm$rel_mse_mcse))))
})

test_that("calc_relative_rmse returns NA for a zero true parameter", {
  est <- rnorm(50)
  expect_equal(calc_relative_rmse(est, 0), list(rel_rmse = NA, rel_rmse_mcse = NA))
})

test_that("calc_relative_rmse MCSE is NA when every estimate hits the true value", {
  rr <- calc_relative_rmse(rep(4, 30), 4)
  expect_equal(rr$rel_rmse, 0)
  expect_true(is.na(rr$rel_rmse_mcse))
})
