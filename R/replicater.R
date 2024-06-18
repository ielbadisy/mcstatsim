#' Replicate expressions with progress reporting
#'
#' Executes an R expression `n` times in a loop, optionally displaying progress and estimated time
#' until completion. This function is particularly useful for simulations where
#' the progress of repetitive tasks is beneficial for monitoring execution time and estimating
#' completion. Progress information includes the percentage completed and an estimated time of
#' arrival (ETA) calculated based on the average time per iteration.
#'
#' @param n An integer specifying the number of times the expression should be replicated.
#' @param expr The expression to be evaluated. This can be any valid R expression. Note that
#' the expression is evaluated in the environment from which `replicater` is called.
#' @param show_progress Logical, indicating whether to show progress and ETA information.
#' Defaults to `TRUE`.
#' @return A list of length `n` containing the results of each evaluation of `expr`.
#' @examples
#' # Simple example without progress (for quick execution)
#' results <- replicater(5, rnorm(1), show_progress = FALSE)
#'
#' # Example with progress - might take longer due to Sys.sleep()
#' results_with_delay <- replicater(10, {Sys.sleep(0.2); rnorm(1)}, show_progress = TRUE)
#' @export
#'
replicater <- function(n, expr, show_progress = TRUE) {
  # start timing
  start_time <- Sys.time()

  # container for results
  results <- vector("list", n)

  for (i in 1:n) {
    # evaluate the expression
    results[[i]] <- eval(substitute(expr), envir = sys.frames()[[1]])

    if (show_progress) {
      # update progress information
      elapsed_time <- Sys.time() - start_time
      avg_time_per_iter <- elapsed_time / i
      eta <- as.numeric(avg_time_per_iter * (n - i))

      # convert ETA to days, hours, minutes, and seconds
      days <- as.integer(floor(eta / 86400))
      hours <- as.integer(floor((eta %% 86400) / 3600))
      minutes <- as.integer(floor((eta %% 3600) / 60))
      seconds <- as.integer(eta %% 60)

      # format the ETA string
      eta_str <- sprintf("%dd %dh %dm %ds", days, hours, minutes, seconds)

      # display the progress
      cat(sprintf("\r[%d/%d] Progress: %d%%, ETA: %s",
                  i, n, floor((i / n) * 100), eta_str), fill = FALSE)
    }
  }
  if (show_progress) {
    cat("\n")
  }

  results
}
