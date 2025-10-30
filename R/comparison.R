#' @export
vec_proxy_equal.phinterval <- function(x, ...) {
  # We can compare the list of matrices, using NULL as a proxy for NA
  data <- vec_data(x)
  data[map_lgl(data, \(x) nrow(x) && is.na(x[1]))] <- list(NULL)
  data
}

# TODO: I'm not sure what to do for this. Maybe just implement `<` directly?
# - Yes see bit64 implementation of the individual comparison operators:
#   https://github.com/r-lib/bit64/blob/main/R/integer64.R
#' @export
vec_proxy_compare.phinterval <- function(x, ...) {
  stop("vec_proxy_compare is not implemented")
}
