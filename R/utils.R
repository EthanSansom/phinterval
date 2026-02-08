#nocov start

is_scalar <- function(x) {
  vec_size(x) == 1L
}

is_instant <- function(x) {
  inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))
}

`%0|%` <- function(lhs, rhs) if (is_empty(lhs)) rhs else lhs

`%|string%` <- function(lhs, rhs) if (is_string(lhs)) lhs else rhs

#nocov end
