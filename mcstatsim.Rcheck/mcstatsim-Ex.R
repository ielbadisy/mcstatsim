pkgname <- "mcstatsim"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "mcstatsim-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('mcstatsim')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("calc_bias")
### * calc_bias

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_bias
### Title: Calculate Bias and Bias Monte Carlo Standard Error
### Aliases: calc_bias

### ** Examples

estimates <- rnorm(100, mean = 50, sd = 10)
true_param <- 50
bias_info <- calc_bias(estimates, true_param)
print(bias_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_bias", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_coverage")
### * calc_coverage

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_coverage
### Title: Calculate Coverage Probability and its Monte Carlo Standard
###   Error
### Aliases: calc_coverage

### ** Examples

set.seed(123) # For reproducibility
estimates <- rnorm(100, mean = 50, sd = 10)
ci_lower <- estimates - 1.96 * 10
ci_upper <- estimates + 1.96 * 10
true_param <- 50
coverage_info <- calc_coverage(ci_lower, ci_upper, true_param)
print(coverage_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_coverage", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_mse")
### * calc_mse

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_mse
### Title: Calculate Mean Squared Error and its Monte Carlo Standard Error
### Aliases: calc_mse

### ** Examples

estimates <- rnorm(100, mean = 50, sd = 10)
true_param <- 50
mse_info <- calc_mse(estimates, true_param)
print(mse_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_mse", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_rejection_rate")
### * calc_rejection_rate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_rejection_rate
### Title: Calculate Rejection Rate and its Monte Carlo Standard Error
### Aliases: calc_rejection_rate

### ** Examples

set.seed(123) # For reproducibility
p_values <- runif(100, min = 0, max = 1) # Simulated p-values
rejection_info <- calc_rejection_rate(p_values)
print(rejection_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_rejection_rate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_relative_bias")
### * calc_relative_bias

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_relative_bias
### Title: Calculate Relative Bias and its Monte Carlo Standard Error
### Aliases: calc_relative_bias

### ** Examples

estimates <- rnorm(100, mean = 50, sd = 10)
true_param <- 50 # Non-zero true parameter
relative_bias_info <- calc_relative_bias(estimates, true_param)
print(relative_bias_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_relative_bias", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_relative_mse")
### * calc_relative_mse

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_relative_mse
### Title: Calculate Relative Mean Squared Error and its Monte Carlo
###   Standard Error
### Aliases: calc_relative_mse

### ** Examples

estimates <- rnorm(100, mean = 50, sd = 10)
true_param <- 50 # Non-zero true parameter
relative_mse_info <- calc_relative_mse(estimates, true_param)
print(relative_mse_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_relative_mse", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_relative_rmse")
### * calc_relative_rmse

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_relative_rmse
### Title: Calculate Relative Root Mean Squared Error and its Monte Carlo
###   Standard Error
### Aliases: calc_relative_rmse

### ** Examples

estimates <- rnorm(100, mean = 50, sd = 10)
true_param <- 50 # Non-zero true parameter
relative_rmse_info <- calc_relative_rmse(estimates, true_param)
print(relative_rmse_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_relative_rmse", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_rmse")
### * calc_rmse

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_rmse
### Title: Calculate Root Mean Squared Error and its Monte Carlo Standard
###   Error
### Aliases: calc_rmse

### ** Examples

estimates <- rnorm(100, mean = 50, sd = 10)
true_param <- 50
rmse_info <- calc_rmse(estimates, true_param)
print(rmse_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_rmse", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_variance")
### * calc_variance

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_variance
### Title: Calculate Variance and its Monte Carlo Standard Error
### Aliases: calc_variance

### ** Examples

estimates <- rnorm(100, mean = 50, sd = 10)
variance_info <- calc_variance(estimates)
print(variance_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_variance", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calc_width")
### * calc_width

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calc_width
### Title: Calculate Average Width of Confidence Intervals and its Monte
###   Carlo Standard Error
### Aliases: calc_width

### ** Examples

set.seed(123) # For reproducibility
estimates <- rnorm(100, mean = 50, sd = 10)
ci_lower <- estimates - 1.96 * 10
ci_upper <- estimates + 1.96 * 10
width_info <- calc_width(ci_lower, ci_upper)
print(width_info)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calc_width", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("combine_df")
### * combine_df

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: combine_df
### Title: Combine Dataframes in a Nested List
### Aliases: combine_df

### ** Examples

df1 <- data.frame(a = 1:3, b = 4:6)
df2 <- data.frame(a = 7:9, b = 10:12)
nested_list <- list(list(df1, df2), list(df1, df2))
combine_df(nested_list)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("combine_df", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mcpmap")
### * mcpmap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mcpmap
### Title: Parallel Map Function using functionals::fmapn
### Aliases: mcpmap

### ** Examples

params <- list(a = 1:3, b = 4:6)
mcpmap(params, function(a, b) a + b, num_cores = 2)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mcpmap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("runsim")
### * runsim

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: runsim
### Title: Run Monte Carlo Simulations in Parallel
### Aliases: runsim

### ** Examples

## Not run: 
##D library(mcstatsim)
##D 
##D # Define a simple simulation function
##D sim_function <- function(a, b) {
##D   Sys.sleep(0.1)  # Simulate a time-consuming process
##D   return(data.frame(result = a + b))
##D }
##D 
##D # Generate a grid of parameters
##D params <- expand.grid(a = 1:3, b = 4:6)
##D 
##D # Run simulations
##D results <- runsim(n = 1, grid_params = params, sim_func = sim_function)
##D print(results)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("runsim", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
