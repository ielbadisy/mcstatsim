library(testthat)

test_that("mcpmap returns aligned results and reports progress", {
  params <- list(a = 1:3, b = 4:6)
  out <- capture.output({
    res <- mcpmap(params, function(a, b) data.frame(sum = a + b), num_cores = 2, show_progress = TRUE)
  })

  expect_true(any(grepl("Dispatching 3 task", out, fixed = TRUE)))
  expect_true(any(grepl("Completed 3 task", out, fixed = TRUE)))
  expect_equal(lapply(res, function(x) x$sum), list(5, 7, 9))
})

test_that("runsim combines replicated results across chunks", {
  grid <- expand.grid(a = 1:2, b = 3:4)
  sim_fun <- function(a, b) data.frame(sum = a + b)

  res <- runsim(n = 2, grid_params = grid, sim_func = sim_fun, show_progress = FALSE, num_cores = 2)

  expect_equal(nrow(res), 8)
  expect_equal(unique(res$ID), c(1, 2))
  expect_true(all(res$sum %in% 4:6))
})
