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

test_that("summarise_sim produces one row per condition with measure + MCSE columns", {
  res <- make_res()
  s <- summarise_sim(
    res,
    by = c("mu", "n_obs"),
    measures = list(
      bias     = ~ calc_bias(est, true_val[1]),
      coverage = ~ calc_coverage(lo, hi, true_val[1])
    )
  )
  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 4)
  expect_equal(names(s), c("mu", "n_obs", "n_sim", "bias", "bias_mcse", "coverage", "coverage_mcse"))
  expect_true(all(s$n_sim == 150))
  expect_true(all(abs(s$bias) < 0.1))
  expect_true(all(s$coverage > 0.8 & s$coverage <= 1))
})

test_that("summarise_sim accepts function-valued measures", {
  res <- make_res()
  s <- summarise_sim(
    res,
    by = "mu",
    measures = list(cov = function(d) calc_coverage(d$lo, d$hi, d$true_val[1]))
  )
  expect_equal(nrow(s), 2)
  expect_true(all(c("coverage", "coverage_mcse") %in% names(s)))
})

test_that("summarise_sim handles a scalar-valued measure and no grouping", {
  res <- make_res()
  s1 <- summarise_sim(res, by = "n_obs", measures = list(mean_width = ~ mean(hi - lo)))
  expect_equal(names(s1), c("n_obs", "n_sim", "mean_width"))
  expect_equal(nrow(s1), 2)
  # narrower intervals with more observations
  expect_lt(s1$mean_width[s1$n_obs == 40], s1$mean_width[s1$n_obs == 20])

  s2 <- summarise_sim(res, measures = list(k = ~ length(est)))
  expect_equal(nrow(s2), 1)
  expect_equal(s2$k, nrow(res))
})

test_that("summarise_sim errors on unknown columns and bad measures", {
  res <- make_res()
  expect_error(summarise_sim(res, by = "nope", measures = list(b = ~ mean(est))),
               "not found in 'data'")
  expect_error(summarise_sim(res, by = "mu", measures = list()),
               "non-empty named list")
  expect_error(summarise_sim(res, by = "mu", measures = list(~ mean(est))),
               "must be named")
  # passing a whole column as the true value -> non-scalar result
  expect_error(summarise_sim(res, measures = list(b = ~ calc_bias(est, true_val))),
               "non-scalar")
})

test_that("summarise_sim flags colliding output columns", {
  res <- make_res()
  expect_error(
    summarise_sim(res, by = "mu",
                  measures = list(a = ~ c(bias = mean(est)), b = ~ c(bias = median(est)))),
    "collide"
  )
})
