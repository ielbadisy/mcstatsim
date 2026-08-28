library(testthat)

test_that("runsim validates its inputs", {
  f <- function(a) data.frame(x = a)
  g <- expand.grid(a = 1:2)
  expect_error(runsim(0, g, f, show_progress = FALSE), "positive integer")
  expect_error(runsim(2.5, g, f, show_progress = FALSE), "positive integer")
  expect_error(runsim(c(1, 2), g, f, show_progress = FALSE), "positive integer")
  expect_error(runsim(2, as.list(g), f, show_progress = FALSE), "data frame")
  expect_error(runsim(2, g[0, , drop = FALSE], f, show_progress = FALSE), "at least one row")
  expect_error(runsim(2, g, "notafun", show_progress = FALSE), "must be a function")
})

test_that("runsim returns one row per condition per replication with an ID column", {
  sim_func <- function(a, b) data.frame(result = a + b)
  params <- expand.grid(a = 1:3, b = 4:6)
  res <- runsim(n = 5, grid_params = params, sim_func = sim_func,
                show_progress = FALSE, num_cores = 1)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5 * nrow(params))
  expect_true("ID" %in% names(res))
  expect_equal(sort(unique(res$ID)), 1:5)
  # replication-major layout: each ID block holds every condition once
  expect_equal(as.integer(table(res$ID)), rep(nrow(params), 5))
  # within replication 1 the conditions appear in grid order
  block1 <- res[res$ID == 1, ]
  expect_equal(block1$result, params$a + params$b)
})

test_that("runsim handles multi-row sim_func output", {
  sim_func <- function(a) data.frame(rep_row = 1:2, val = a * (1:2))
  params <- expand.grid(a = c(10, 20))
  res <- runsim(n = 3, grid_params = params, sim_func = sim_func,
                show_progress = FALSE, num_cores = 1)

  expect_equal(nrow(res), 3 * 2 * 2)
  expect_equal(as.integer(table(res$ID)), rep(4, 3))
})

test_that("runsim errors when sim_func does not return a data frame", {
  params <- expand.grid(a = 1:2)
  expect_error(
    runsim(2, params, function(a) a + 1, show_progress = FALSE, num_cores = 1),
    "must return a data frame"
  )
})
