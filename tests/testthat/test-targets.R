library(testthat)

test_that("calcrelativermse is the sqrt of calcrelativemse", {
  set.seed(1)
  est <- rnorm(200, mean = 5, sd = 2)
  tp <- 5

  rm <- calcrelativemse(est, tp, digits = NULL)
  rr <- calcrelativermse(est, tp, digits = NULL)

  expect_equal(rr$rel_rmse, sqrt(rm$rel_mse))
})

test_that("calcrelativermse MCSE follows the delta method, not sqrt() of the MSE MCSE", {
  set.seed(2)
  est <- rnorm(200, mean = 10, sd = 3)
  tp <- 10

  rm <- calcrelativemse(est, tp, digits = NULL)
  rr <- calcrelativermse(est, tp, digits = NULL)

  expect_equal(rr$rel_rmse_mcse, rm$rel_mse_mcse / (2 * sqrt(rm$rel_mse)))
  # the old implementation returned sqrt(rel_mse_mcse); make sure we no longer do
  expect_false(isTRUE(all.equal(rr$rel_rmse_mcse, sqrt(rm$rel_mse_mcse))))
})

test_that("calc* round to `digits` by default and honour digits = NULL", {
  set.seed(3)
  est <- rnorm(500, mean = 2, sd = 1)
  b_default <- calcbias(est, 2)
  b_raw <- calcbias(est, 2, digits = NULL)
  b_2 <- calcbias(est, 2, digits = 2)

  expect_equal(b_default$bias, round(b_raw$bias, 4))
  expect_equal(b_default$bias_mcse, round(b_raw$bias_mcse, 4))
  expect_equal(b_2$bias, round(b_raw$bias, 2))
  expect_false(identical(b_default$bias, b_raw$bias))  # rounding actually happened
  expect_error(calcbias(est, 2, digits = "x"), "single number or NULL")
})

test_that("calcrelativermse returns NA for a zero true parameter", {
  est <- rnorm(50)
  expect_equal(calcrelativermse(est, 0), list(rel_rmse = NA, rel_rmse_mcse = NA))
})

test_that("calcrelativermse MCSE is NA when every estimate hits the true value", {
  rr <- calcrelativermse(rep(4, 30), 4)
  expect_equal(rr$rel_rmse, 0)
  expect_true(is.na(rr$rel_rmse_mcse))
})
