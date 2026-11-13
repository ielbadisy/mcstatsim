#' Deprecated functions
#'
#' These functions were renamed to glued lowercase names (no underscores). The
#' old names still work but are deprecated and will be removed in a future
#' release.
#'
#' @param ... Arguments passed to the new function.
#' @name mcstatsim-deprecated
#' @keywords internal
NULL

#' @rdname mcstatsim-deprecated
#' @export
calc_bias <- function(...) {
  .Deprecated("calcbias")
  calcbias(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_coverage <- function(...) {
  .Deprecated("calccoverage")
  calccoverage(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_mse <- function(...) {
  .Deprecated("calcmse")
  calcmse(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_rejection_rate <- function(...) {
  .Deprecated("calcrejectionrate")
  calcrejectionrate(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_relative_bias <- function(...) {
  .Deprecated("calcrelativebias")
  calcrelativebias(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_relative_mse <- function(...) {
  .Deprecated("calcrelativemse")
  calcrelativemse(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_relative_rmse <- function(...) {
  .Deprecated("calcrelativermse")
  calcrelativermse(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_rmse <- function(...) {
  .Deprecated("calcrmse")
  calcrmse(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_variance <- function(...) {
  .Deprecated("calcvariance")
  calcvariance(...)
}

#' @rdname mcstatsim-deprecated
#' @export
calc_width <- function(...) {
  .Deprecated("calcwidth")
  calcwidth(...)
}

#' @rdname mcstatsim-deprecated
#' @export
check_estimates <- function(...) {
  .Deprecated("checkestimates")
  checkestimates(...)
}

#' @rdname mcstatsim-deprecated
#' @export
combine_df <- function(...) {
  .Deprecated("combinedf")
  combinedf(...)
}

#' @rdname mcstatsim-deprecated
#' @export
failure_summary <- function(...) {
  .Deprecated("failuresummary")
  failuresummary(...)
}

#' @rdname mcstatsim-deprecated
#' @export
mcse_target <- function(...) {
  .Deprecated("mcsetarget")
  mcsetarget(...)
}

#' @rdname mcstatsim-deprecated
#' @export
plot_convergence <- function(...) {
  .Deprecated("plotconvergence")
  plotconvergence(...)
}

#' @rdname mcstatsim-deprecated
#' @export
plot_performance <- function(...) {
  .Deprecated("plotperformance")
  plotperformance(...)
}

#' @rdname mcstatsim-deprecated
#' @export
plot_zip <- function(...) {
  .Deprecated("plotzip")
  plotzip(...)
}

#' @rdname mcstatsim-deprecated
#' @export
summarise_sim <- function(...) {
  .Deprecated("summarisesim")
  summarisesim(...)
}
