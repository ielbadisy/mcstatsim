library(testthat)

test_that("save_path produces the same result as an uninterrupted run", {
  sim_func <- function(mu, sd) data.frame(xbar = mean(rnorm(15, mu, sd)))
  params <- expand.grid(mu = c(0, 3), sd = c(1, 2))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(path, paste0(path, ".tmp"))), add = TRUE)

  plain <- runsim(8, params, sim_func, show_progress = FALSE, num_cores = 1, seed = 5)
  ckpt <- runsim(8, params, sim_func, show_progress = FALSE, num_cores = 1, seed = 5,
                 save_path = path, checkpoint_every = 3)

  expect_equal(plain$xbar, ckpt$xbar)
  expect_true(file.exists(path))
})

test_that("a run resumes from an existing checkpoint and skips completed replications", {
  calls <- new.env()
  calls$k <- 0L
  sim_func <- function(a) data.frame(x = a)
  params <- expand.grid(a = 1:3)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(path, paste0(path, ".tmp"))), add = TRUE)

  # first run: only 4 of 10 replications, simulated by a hand-written checkpoint
  full <- runsim(10, params, sim_func, show_progress = FALSE, num_cores = 1, seed = 1,
                 save_path = path, checkpoint_every = 2)
  expect_equal(nrow(full), 30)

  ck <- readRDS(path)
  ck$done_reps <- 1:4
  ck$values[which(rep(1:10, each = 3) > 4)] <- list(NULL)
  saveRDS(ck, path)

  resumed <- runsim(10, params, sim_func, show_progress = FALSE, num_cores = 1, seed = 1,
                    save_path = path, checkpoint_every = 2)
  expect_equal(nrow(resumed), 30)
  expect_equal(sort(unique(resumed$ID)), 1:10)
  expect_equal(resumed$x[order(resumed$ID)], full$x[order(full$ID)])
})

test_that("resuming with mismatched parameters is refused", {
  sim_func <- function(a) data.frame(x = a)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(path, paste0(path, ".tmp"))), add = TRUE)

  runsim(4, expand.grid(a = 1:2), sim_func, show_progress = FALSE, num_cores = 1, save_path = path)

  expect_error(
    runsim(5, expand.grid(a = 1:2), sim_func, show_progress = FALSE, num_cores = 1, save_path = path),
    "does not match this call.*n"
  )
  expect_error(
    runsim(4, expand.grid(a = 1:3), sim_func, show_progress = FALSE, num_cores = 1, save_path = path),
    "does not match this call"
  )
  expect_error(
    runsim(4, expand.grid(a = 1:2), sim_func, show_progress = FALSE, num_cores = 1, seed = 9, save_path = path),
    "seed"
  )
})

test_that("on_error = 'stop' still aborts under checkpointing but keeps prior progress", {
  sim_func <- function(a) if (a == 2) stop("boom") else data.frame(x = a)
  params <- expand.grid(a = 1:3)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(c(path, paste0(path, ".tmp"))), add = TRUE)

  expect_error(
    runsim(6, params, sim_func, show_progress = FALSE, num_cores = 1,
           save_path = path, checkpoint_every = 2),
    "errored.*boom.*Progress saved"
  )
  expect_true(file.exists(path))

  # fix sim_func, resume: the checkpoint holds the failed reps as not-done
  fixed <- function(a) data.frame(x = a)
  res <- runsim(6, params, fixed, show_progress = FALSE, num_cores = 1,
                save_path = path, checkpoint_every = 2)
  expect_equal(nrow(res), 18)
})

test_that("checkpoint_every must be a positive integer", {
  sim_func <- function(a) data.frame(x = a)
  params <- expand.grid(a = 1:2)
  expect_error(runsim(2, params, sim_func, show_progress = FALSE, save_path = tempfile(), checkpoint_every = 0),
               "positive integer")
  expect_error(runsim(2, params, sim_func, show_progress = FALSE, save_path = tempfile(), checkpoint_every = 2.5),
               "positive integer")
})
