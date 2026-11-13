# Internal: round the numeric parts of `x` (a scalar, vector, or list of those)
# to `digits` places, except that values with 0 < |x| < 1 keep `digits`
# significant digits, so tiny quantities are never rounded to 0. For
# 0.1 <= |x| < 1 the two rules agree. `digits = NULL` returns `x` untouched. Non-numeric parts
# (character, logical, NA of other types) pass through unchanged.
.mcstatsim_round <- function(x, digits) {
  if (is.null(digits)) return(x)
  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits)) {
    stop("'digits' must be a single number or NULL.")
  }
  if (is.list(x)) return(lapply(x, .mcstatsim_round, digits = digits))
  if (is.numeric(x)) {
    out <- round(x, digits)
    small <- !is.na(x) & abs(x) < 1
    out[small] <- signif(x[small], digits)
    return(out)
  }
  x
}
