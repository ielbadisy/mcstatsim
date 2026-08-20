#' Calculate Bias and Bias Monte Carlo Standard Error
#'
#' This function computes the bias and the Monte Carlo Standard Error (MCSE) of the bias
#' for a set of estimates relative to a true parameter value. The bias is the difference
#' between the mean of the estimates and the true parameter. The MCSE of the bias is calculated
#' as the square root of the variance of the estimates divided by the number of estimates,
#' providing a measure of the precision of the bias estimate.
#'
#' @param estimates A numeric vector of estimates from the simulation or sampling process.
#' @param true_param The true parameter value that the estimates are intended to approximate.
#' @return A list with two components: `bias`, the calculated bias of the estimates, and `bias_mcse`,
#' the Monte Carlo Standard Error of the bias, indicating the uncertainty associated with the bias estimate.
#' @examples
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' true_param <- 50
#' bias_info <- calc_bias(estimates, true_param)
#' print(bias_info)
#' @export
#' @importFrom stats sd var
calc_bias <- function(estimates, true_param) {
  bias <- mean(estimates) - true_param
  bias_mcse <- sqrt(var(estimates) / length(estimates))
  return(list(bias = bias, bias_mcse = bias_mcse))
}


#' Calculate Variance and its Monte Carlo Standard Error
#'
#' Computes the sample variance of a set of estimates and the Monte Carlo Standard Error (MCSE)
#' for the variance. The MCSE is adjusted by the sample kurtosis to account for the shape
#' of the distribution of the estimates. This function is particularly useful in simulation studies
#' where understanding the variability of an estimator and the precision of this variability estimate
#' is crucial.
#'
#' @param estimates A numeric vector of estimates from a simulation or sampling process.
#' @return A list containing two elements: `variance`, the calculated sample variance of the estimates,
#' and `variance_mcse`, the Monte Carlo Standard Error of the variance. The MCSE provides a measure of
#' the uncertainty associated with the variance estimate, adjusted for kurtosis.
#' @examples
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' variance_info <- calc_variance(estimates)
#' print(variance_info)
#' @export
#' @importFrom stats sd var
calc_variance <- function(estimates) {
  variance <- var(estimates)
  kurtosis <- (1 / (length(estimates) * sd(estimates)^4)) * sum((estimates - mean(estimates))^4)
  variance_mcse <- variance * sqrt((kurtosis - 1) / length(estimates))
  return(list(variance = variance, variance_mcse = variance_mcse))
}


#' Calculate Mean Squared Error and its Monte Carlo Standard Error
#'
#' Computes the Mean Squared Error (MSE) of a set of estimates relative to a true parameter value,
#' along with the Monte Carlo Standard Error (MCSE) for the MSE. The MCSE takes into account the
#' variance, skewness, and kurtosis of the estimates to provide a more accurate measure of uncertainty.
#' This function is useful for assessing the accuracy of simulation or estimation methods by comparing
#' the squared deviations of estimated values from a known parameter.
#'
#' @param estimates A numeric vector of estimates from a simulation or sampling process.
#' @param true_param The true parameter value that the estimates are intended to approximate.
#' @return A list with two components: `mse`, the calculated Mean Squared Error of the estimates,
#' and `mse_mcse`, the Monte Carlo Standard Error of the MSE, offering insight into the reliability
#' of the MSE calculation.
#' @examples
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' true_param <- 50
#' mse_info <- calc_mse(estimates, true_param)
#' print(mse_info)
#' @export
#' @importFrom stats sd var

calc_mse <- function(estimates, true_param) {
  mse <- mean((estimates - true_param)^2)
  sd_t <- sd(estimates)
  kurtosis <- (1 / (length(estimates) * sd_t^4)) * sum((estimates - mean(estimates))^4)
  skewness <- (1 / (length(estimates) * sd_t^3)) * sum((estimates - mean(estimates))^3)
  mse_mcse <- sqrt((1 / length(estimates)) * (sd_t^4 * (kurtosis - 1) + 4 * sd_t^3 * skewness * (mean(estimates) - true_param) + 4 * var(estimates) * (mean(estimates) - true_param)^2))
  return(list(mse = mse, mse_mcse = mse_mcse))
}


#' Calculate Root Mean Squared Error and its Monte Carlo Standard Error
#'
#' Computes the Root Mean Squared Error (RMSE) of a set of estimates relative to a true parameter value,
#' along with the Monte Carlo Standard Error (MCSE) for the RMSE. The RMSE is a measure of the accuracy
#' of the estimates, representing the square root of the average squared differences between the estimated
#' values and the true parameter. The MCSE for the RMSE is calculated using jackknife estimates, providing
#' an assessment of the uncertainty associated with the RMSE value.
#'
#' @param estimates A numeric vector of estimates from a simulation or sampling process.
#' @param true_param The true parameter value that the estimates are intended to approximate.
#' @return A list with two components: `rmse`, the calculated Root Mean Squared Error of the estimates,
#' and `rmse_mcse`, the Monte Carlo Standard Error of the RMSE. This MCSE is derived from jackknife
#' estimates, offering insight into the reliability of the RMSE calculation.
#' @examples
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' true_param <- 50
#' rmse_info <- calc_rmse(estimates, true_param)
#' print(rmse_info)
#' @export
#' @importFrom stats sd var

calc_rmse <- function(estimates, true_param) {
  mse <- mean((estimates - true_param)^2)
  rmse <- sqrt(mse)
  # Compute jackknife estimates
  t_bar <- mean(estimates)
  t_bar_j <- (1 / (length(estimates) - 1)) * (length(estimates) * t_bar - estimates)
  bias_j_sq <- (t_bar_j - true_param)^2
  s_sq_t_j <- (1 / (length(estimates) - 2)) * ((length(estimates) - 1) * var(estimates) - (length(estimates) / (length(estimates) - 1)) * (estimates - t_bar)^2)
  rmse_j <- sqrt(bias_j_sq + s_sq_t_j)
  rmse_mcse <- sqrt(((length(estimates) - 1) / length(estimates)) * sum((rmse_j - rmse)^2))
  return(list(rmse = rmse, rmse_mcse = rmse_mcse))
}



#' Calculate Coverage Probability and its Monte Carlo Standard Error
#'
#' Computes the coverage probability of a confidence interval, defined as the proportion
#' of times the true parameter value falls within the calculated lower and upper bounds
#' across a set of simulations. Additionally, calculates the Monte Carlo Standard Error (MCSE)
#' of the coverage probability to assess the uncertainty associated with this coverage estimate.
#' This function is useful for evaluating the accuracy and reliability of confidence intervals
#' generated by statistical models or estimation procedures.
#'
#' @param lower_bound A numeric vector of lower bounds of confidence intervals.
#' @param upper_bound A numeric vector of upper bounds of confidence intervals, corresponding to `lower_bound`.
#' @param true_param The true parameter value that the confidence intervals are intended to estimate.
#' @return A list with two components: `coverage`, the calculated coverage probability of the confidence intervals,
#' and `coverage_mcse`, the Monte Carlo Standard Error of the coverage. This MCSE provides a measure of the precision
#' of the coverage probability estimate.
#' @examples
#' set.seed(123) # For reproducibility
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' ci_lower <- estimates - 1.96 * 10
#' ci_upper <- estimates + 1.96 * 10
#' true_param <- 50
#' coverage_info <- calc_coverage(ci_lower, ci_upper, true_param)
#' print(coverage_info)
#' @export
#' @importFrom stats sd var

calc_coverage <- function(lower_bound, upper_bound, true_param) {
  coverage <- mean(lower_bound <= true_param & true_param <= upper_bound)
  coverage_mcse <- sqrt((coverage * (1 - coverage)) / length(lower_bound))
  return(list(coverage = coverage, coverage_mcse = coverage_mcse))
}


#' Calculate Average Width of Confidence Intervals and its Monte Carlo Standard Error
#'
#' This function computes the average width of a set of confidence intervals, represented by their
#' lower and upper bounds, along with the Monte Carlo Standard Error (MCSE) of this average width.
#' The average width provides insight into the precision of the estimation process, with narrower
#' intervals typically indicating more precise estimates. The MCSE of the width offers a measure of
#' the uncertainty associated with the average width calculation, useful for assessing the variability
#' of interval estimates across simulations or bootstrap samples.
#'
#' @param lower_bound A numeric vector of lower bounds of confidence intervals.
#' @param upper_bound A numeric vector of upper bounds of confidence intervals, corresponding to `lower_bound`.
#' @return A list with two components: `width`, the average width of the confidence intervals, and
#' `width_mcse`, the Monte Carlo Standard Error of the average width. This MCSE provides a quantification
#' of the uncertainty in the average width estimate.
#' @examples
#' set.seed(123) # For reproducibility
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' ci_lower <- estimates - 1.96 * 10
#' ci_upper <- estimates + 1.96 * 10
#' width_info <- calc_width(ci_lower, ci_upper)
#' print(width_info)
#' @export
#' @importFrom stats sd var
calc_width <- function(lower_bound, upper_bound) {
  width <- mean(upper_bound - lower_bound)
  width_mcse <- sqrt(var(upper_bound - lower_bound) / length(lower_bound))
  return(list(width = width, width_mcse = width_mcse))
}


#' Calculate Rejection Rate and its Monte Carlo Standard Error
#'
#' Computes the rejection rate of hypotheses tests based on a vector of p-values and a specified
#' significance level (alpha). The rejection rate is the proportion of p-values that are lower than
#' alpha, indicating significant results. Additionally, the function calculates the Monte Carlo
#' Standard Error (MCSE) for the rejection rate, which quantifies the uncertainty associated with
#' the estimated rejection rate. This function is useful for assessing the overall type I error rate
#' or the power of a statistical test across multiple simulations or experimental replications.
#'
#' @param p_values A numeric vector of p-values from multiple hypothesis tests.
#' @param alpha The significance level used to determine if a p-value indicates a significant result.
#' Default is 0.05.
#' @return A list with two components: `rejection_rate`, the proportion of tests that resulted in
#' rejection of the null hypothesis, and `rejection_rate_mcse`, the Monte Carlo Standard Error of the
#' rejection rate, providing an estimate of its variability.
#' @examples
#' set.seed(123) # For reproducibility
#' p_values <- runif(100, min = 0, max = 1) # Simulated p-values
#' rejection_info <- calc_rejection_rate(p_values)
#' print(rejection_info)
#' @export
#' @importFrom stats sd var
calc_rejection_rate <- function(p_values, alpha = 0.05) {
  rejection_rate <- mean(p_values < alpha)
  rejection_rate_mcse <- sqrt((rejection_rate * (1 - rejection_rate)) / length(p_values))
  return(list(rejection_rate = rejection_rate, rejection_rate_mcse = rejection_rate_mcse))
}


#' Calculate Relative Bias and its Monte Carlo Standard Error
#'
#' Computes the relative bias of a set of estimates with respect to a true parameter value,
#' along with the Monte Carlo Standard Error (MCSE) of the relative bias. Relative bias is
#' the ratio of the mean of the estimates to the true parameter, providing a scale-independent
#' measure of bias. This function is particularly useful for evaluating the accuracy of estimates
#' in situations where the magnitude of the true parameter is crucial to the interpretation of bias.
#' The function gracefully handles cases where the true parameter is zero by returning `NA` for
#' both relative bias and its MCSE, avoiding division by zero errors.
#'
#' @param estimates A numeric vector of estimates from a simulation or sampling process.
#' @param true_param The true parameter value that the estimates are intended to approximate.
#' Note that `true_param` must not be zero, as relative bias calculation involves division by
#' the true parameter value.
#' @return A list with two components: `rel_bias`, the calculated relative bias of the estimates, and
#' `rel_bias_mcse`, the Monte Carlo Standard Error of the relative bias. If `true_param` is zero,
#' both `rel_bias` and `rel_bias_mcse` will be `NA`.
#' @examples
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' true_param <- 50 # Non-zero true parameter
#' relative_bias_info <- calc_relative_bias(estimates, true_param)
#' print(relative_bias_info)
#' @export
#' @importFrom stats sd var
calc_relative_bias <- function(estimates, true_param) {
  if (true_param == 0) {
    return(list(rel_bias = NA, rel_bias_mcse = NA))
  }
  relative_bias <- mean(estimates) / true_param
  relative_bias_mcse <- sqrt(var(estimates) / (length(estimates) * true_param^2))
  return(list(rel_bias = relative_bias, rel_bias_mcse = relative_bias_mcse))
}


#' Calculate Relative Mean Squared Error and its Monte Carlo Standard Error
#'
#' Computes the Relative Mean Squared Error (Relative MSE) of a set of estimates with respect to a true
#' parameter value, along with the Monte Carlo Standard Error (MCSE) of the Relative MSE. The Relative
#' MSE is a normalized measure of error that scales the Mean Squared Error (MSE) by the square of the
#' true parameter value, making it particularly useful for comparing the accuracy of estimates across
#' different scales. The function also calculates the MCSE for the Relative MSE, taking into account the
#' variance, skewness, and kurtosis of the estimates to provide a measure of uncertainty. The function
#' returns `NA` for both Relative MSE and its MCSE if the true parameter is zero, to avoid division by zero.
#'
#' @param estimates A numeric vector of estimates from a simulation or sampling process.
#' @param true_param The true parameter value that the estimates are intended to approximate.
#' Note that `true_param` must not be zero, as the calculation involves division by the true parameter value.
#' @return A list with two components: `rel_mse`, the calculated Relative Mean Squared Error of the estimates,
#' and `rel_mse_mcse`, the Monte Carlo Standard Error of the Relative MSE. If `true_param` is zero,
#' both `rel_mse` and `rel_mse_mcse` will be `NA`.
#' @examples
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' true_param <- 50 # Non-zero true parameter
#' relative_mse_info <- calc_relative_mse(estimates, true_param)
#' print(relative_mse_info)
#' @export
#' @importFrom stats sd var
calc_relative_mse <- function(estimates, true_param) {
  if (true_param == 0) {
    return(list(rel_mse = NA, rel_mse_mcse = NA))
  }
  bias <- mean(estimates) - true_param
  variance <- var(estimates)
  relative_mse <- (bias^2 + variance) / true_param^2
  s_t <- sd(estimates)
  g_t <- (1/(length(estimates) * s_t^3)) * sum((estimates - mean(estimates))^3)
  k_t <- (1/(length(estimates) * s_t^4)) * sum((estimates - mean(estimates))^4)
  relative_mse_mcse <- sqrt((1 / (length(estimates) * true_param^2)) * (s_t^4 * (k_t - 1) + 4 * s_t^3 * g_t * bias + 4 * variance * bias^2))
  return(list(rel_mse = relative_mse, rel_mse_mcse = relative_mse_mcse))
}


#' Calculate Relative Root Mean Squared Error and its Monte Carlo Standard Error
#'
#' Computes the Relative Root Mean Squared Error (Relative RMSE) of a set of estimates with respect
#' to a true parameter value. The Relative RMSE is derived from the Relative Mean Squared Error (MSE),
#' providing a scale-independent measure of error that facilitates comparisons across different scales
#' of the true parameter. This function is especially useful for evaluating the accuracy of estimates
#' when the magnitude of the true parameter varies significantly across different scenarios. The function
#' gracefully handles cases where the true parameter is zero by returning `NA` for both Relative RMSE
#' and its MCSE, to avoid division by zero. The MCSE for the Relative RMSE is not directly computed
#' in this function and is marked as a placeholder for future implementation.
#'
#' @param estimates A numeric vector of estimates from a simulation or sampling process.
#' @param true_param The true parameter value that the estimates are intended to approximate.
#' Note that `true_param` must not be zero, as the calculation involves division by the true parameter value.
#' @return A list with two components: `rel_rmse`, the calculated Relative Root Mean Squared Error of the
#' estimates, and `rel_rmse_mcse`, the Monte Carlo Standard Error of the Relative RMSE. The MCSE is
#' currently not calculated and returned as `NA`. This is a placeholder for future implementation.
#' @examples
#' estimates <- rnorm(100, mean = 50, sd = 10)
#' true_param <- 50 # Non-zero true parameter
#' relative_rmse_info <- calc_relative_rmse(estimates, true_param)
#' print(relative_rmse_info)
#' @export
#' @importFrom stats sd var
calc_relative_rmse <- function(estimates, true_param) {
  if (true_param == 0) {
    return(list(rel_rmse = NA, rel_rmse_mcse = NA))
  }

  relative_mse <- calc_relative_mse(estimates, true_param)$rel_mse
  relative_rmse <- sqrt(relative_mse)

  relative_mse_mcse <- calc_relative_mse(estimates, true_param)$rel_mse_mcse
  relative_rmse_mcse <- sqrt(relative_mse_mcse)

  return(list(rel_rmse = relative_rmse, rel_rmse_mcse = relative_rmse_mcse))
}
