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

test_that("runsim prepends grid columns the sim_func did not return", {
  sim_func <- function(a, b) data.frame(result = a + b)
  params <- expand.grid(a = 1:3, b = 4:5)
  res <- runsim(2, params, sim_func, show_progress = FALSE, num_cores = 1)
  expect_equal(names(res), c("a", "b", "result", "ID"))
  block1 <- res[res$ID == 1, ]
  expect_equal(block1$a, params$a)
  expect_equal(block1$b, params$b)
  expect_equal(block1$result, params$a + params$b)
})

test_that("runsim does not duplicate a grid column the sim_func already returns", {
  sim_func <- function(a) data.frame(a = a, result = a^2)
  params <- expand.grid(a = 1:3)
  res <- runsim(2, params, sim_func, show_progress = FALSE, num_cores = 1)
  expect_equal(names(res), c("a", "result", "ID"))
  expect_equal(sum(names(res) == "a"), 1)
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

test_that("runsim rejects a malformed seed", {
  params <- expand.grid(a = 1:2)
  f <- function(a) data.frame(x = rnorm(1))
  expect_error(runsim(2, params, f, show_progress = FALSE, seed = c(1, 2)), "single number or NULL")
  expect_error(runsim(2, params, f, show_progress = FALSE, seed = "1"), "single number or NULL")
  expect_error(runsim(2, params, f, show_progress = FALSE, seed = NA_real_), "single number or NULL")
})

test_that("a supplied seed makes runsim reproducible and independent of core count", {
  sim_func <- function(mu, sd) data.frame(xbar = mean(rnorm(20, mu, sd)))
  params <- expand.grid(mu = c(0, 5), sd = c(1, 2))

  a <- runsim(10, params, sim_func, show_progress = FALSE, num_cores = 1, seed = 42)
  b <- runsim(10, params, sim_func, show_progress = FALSE, num_cores = 1, seed = 42)
  expect_identical(a$xbar, b$xbar)
  expect_equal(attr(a, "seed"), 42)

  c <- runsim(10, params, sim_func, show_progress = FALSE, num_cores = 2, seed = 42)
  expect_equal(a$xbar, c$xbar)

  d <- runsim(10, params, sim_func, show_progress = FALSE, num_cores = 1, seed = 7)
  expect_false(isTRUE(all.equal(a$xbar, d$xbar)))
})

test_that("runsim with a seed does not disturb the caller's RNG state", {
  sim_func <- function(a) data.frame(x = rnorm(1))
  params <- expand.grid(a = 1:3)

  set.seed(123)
  before <- .Random.seed
  ref <- runif(5)

  set.seed(123)
  invisible(runsim(4, params, sim_func, show_progress = FALSE, num_cores = 1, seed = 99))
  after <- .Random.seed
  expect_identical(after, before)
  expect_identical(runif(5), ref)
})

test_that("runsim without a seed leaves results random and carries no seed attribute", {
  sim_func <- function(a) data.frame(x = rnorm(1))
  params <- expand.grid(a = 1:3)
  a <- runsim(20, params, sim_func, show_progress = FALSE, num_cores = 1)
  b <- runsim(20, params, sim_func, show_progress = FALSE, num_cores = 1)
  expect_null(attr(a, "seed"))
  expect_false(isTRUE(all.equal(a$x, b$x)))
})

test_that("on_error = 'stop' (default) aborts when sim_func errors", {
  sim_func <- function(a) if (a == 2) stop("boom") else data.frame(x = a)
  params <- expand.grid(a = 1:3)
  expect_error(
    runsim(2, params, sim_func, show_progress = FALSE, num_cores = 1),
    "errored on 2 of 6 job\\(s\\).*boom"
  )
})

test_that("on_error = 'warn' drops failed jobs, keeps the rest, logs errors", {
  sim_func <- function(a) if (a == 2) stop("boom") else data.frame(x = a)
  params <- expand.grid(a = 1:3)
  expect_warning(
    res <- runsim(4, params, sim_func, show_progress = FALSE, num_cores = 1, on_error = "warn"),
    "errored on 4 of 12"
  )
  expect_equal(nrow(res), 8)
  expect_false(any(res$x == 2))
  err <- attr(res, "errors")
  expect_s3_class(err, "data.frame")
  expect_equal(nrow(err), 4)
  expect_true(all(err$a == 2))
  expect_equal(sort(unique(err$ID)), 1:4)
  expect_true(all(grepl("boom", err$error)))
})

test_that("on_error = 'omit' drops failed jobs silently", {
  sim_func <- function(a) if (a == 2) stop("boom") else data.frame(x = a)
  params <- expand.grid(a = 1:3)
  expect_silent(
    res <- runsim(2, params, sim_func, show_progress = FALSE, num_cores = 1, on_error = "omit")
  )
  expect_equal(nrow(res), 4)
  expect_equal(nrow(attr(res, "errors")), 2)
})

test_that("runsim aborts when every job fails even under on_error = 'omit'", {
  sim_func <- function(a) stop("always")
  params <- expand.grid(a = 1:2)
  expect_error(
    runsim(3, params, sim_func, show_progress = FALSE, num_cores = 1, on_error = "omit"),
    "errored on all 6 job\\(s\\)"
  )
})

test_that("runsim captures sim_func warnings without failing", {
  sim_func <- function(a) {
    if (a == 1) warning("mild warning")
    data.frame(x = a)
  }
  params <- expand.grid(a = 1:2)
  res <- runsim(3, params, sim_func, show_progress = FALSE, num_cores = 1)
  expect_equal(nrow(res), 6)
  w <- attr(res, "warnings")
  expect_s3_class(w, "data.frame")
  expect_equal(nrow(w), 3)
  expect_true(all(w$a == 1))
  expect_true(all(grepl("mild warning", w$warning)))
})

test_that("auto backend avoids fork when RcppParallel is loaded or on Windows", {
  r <- mcstatsim:::.runsim_resolve_backend
  expect_identical(r("auto", 1, "unix", "base"), "sequential")
  expect_identical(r("auto", 2, "unix", c("base", "stats"), threaded = FALSE), "fork")
  expect_identical(r("auto", 2, "unix", c("base", "stats"), threaded = TRUE), "psock")
  expect_identical(r("fork", 2, "unix", "base", threaded = TRUE), "fork")
  expect_identical(r("auto", 2, "unix", c("base", "RcppParallel")), "psock")
  expect_identical(r("auto", 2, "windows", "base"), "psock")
  expect_identical(r("fork", 2, "unix", "RcppParallel"), "fork")
  expect_error(r("fork", 2, "windows", "base"), "not available on Windows")
})

test_that("psock backend gives the same seeded results as fork and sequential", {
  skip_on_cran()
  skip_on_os("windows")
  helper_scale <- 3
  helper_draw <- function(n_obs) rnorm(n_obs) * helper_scale
  assign("helper_scale", helper_scale, envir = globalenv())
  assign("helper_draw", helper_draw, envir = globalenv())
  on.exit(rm(list = c("helper_scale", "helper_draw"), envir = globalenv()), add = TRUE)
  # sim_func defined in the global environment, using global helpers
  sim <- eval(quote(function(n_obs) {
    x <- helper_draw(n_obs)
    if (n_obs == 10) warning("small n")
    data.frame(est = mean(x), sdv = sd(x))
  }), globalenv())
  grid <- expand.grid(n_obs = c(10, 50))
  seq_res <- runsim(6, grid, sim, show_progress = FALSE, num_cores = 1, seed = 42)
  fork_res <- runsim(6, grid, sim, show_progress = FALSE, num_cores = 2, seed = 42, backend = "fork")
  psock_res <- runsim(6, grid, sim, show_progress = FALSE, num_cores = 2, seed = 42, backend = "psock")
  expect_identical(fork_res, seq_res)
  expect_identical(psock_res, seq_res)
  expect_equal(nrow(attr(psock_res, "warnings")), 6L)

  # error capture works on psock workers too
  bad <- function(n_obs) if (n_obs == 10) stop("boom") else data.frame(est = 1)
  out <- runsim(2, grid, bad, show_progress = FALSE, num_cores = 2, seed = 1,
                backend = "psock", on_error = "omit")
  expect_equal(nrow(out), 2L)
  expect_match(attr(out, "errors")$error, "boom")
})

test_that(".runsim_threaded_runtime reads thread count and mapped runtimes", {
  fake <- function(threads, libs) {
    d <- tempfile("proc")
    dir.create(d)
    writeLines(c("Name:\tR", paste0("Threads:\t", threads)), file.path(d, "status"))
    writeLines(paste("7f00-7f01 r-xp 00000000 00:00 0 /usr/lib/", libs), file.path(d, "maps"))
    d
  }
  f <- mcstatsim:::.runsim_threaded_runtime
  expect_true(f(fake(8, "libgomp.so.1.0.0")))
  expect_true(f(fake(3, "libopenblasp-r0.3.so")))
  expect_false(f(fake(1, "libgomp.so.1.0.0")))
  expect_false(f(fake(4, "libR.so")))
  expect_false(f(file.path(tempdir(), "no-such-proc")))
})

test_that("an error inside a parallel worker surfaces as an R error with its message", {
  skip_on_cran()
  params <- data.frame(a = 1:3)
  f <- function(a) {
    if (a == 2) stop("boom in worker")
    data.frame(x = a)
  }
  backends <- if (.Platform$OS.type == "windows") "psock" else c("fork", "psock")
  for (b in backends) {
    expect_error(
      runsim(3, params, f, show_progress = FALSE, num_cores = 2, seed = 1, backend = b),
      "errored on 3 of 9 job\\(s\\).*boom in worker"
    )
  }
})

test_that("a fork worker that dies is reported instead of lost", {
  skip_on_cran()
  skip_on_os("windows")
  params <- data.frame(a = 1:3)
  f <- function(a) {
    if (a == 2) tools::pskill(Sys.getpid(), tools::SIGKILL)
    data.frame(x = a)
  }
  expect_error(
    suppressWarnings(runsim(2, params, f, show_progress = FALSE, num_cores = 2,
                            seed = 1, backend = "fork")),
    "worker process died"
  )
})
