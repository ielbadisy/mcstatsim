# Internal: round the numeric parts of `x` (a scalar, vector, or list of those)
# to `digits` places. `digits = NULL` returns `x` untouched. Non-numeric parts
# (character, logical, NA of other types) pass through unchanged.
.mcstatsim_round <- function(x, digits) {
  if (is.null(digits)) return(x)
  if (!is.numeric(digits) || length(digits) != 1 || is.na(digits)) {
    stop("'digits' must be a single number or NULL.")
  }
  if (is.list(x)) return(lapply(x, .mcstatsim_round, digits = digits))
  if (is.numeric(x)) return(round(x, digits))
  x
}
