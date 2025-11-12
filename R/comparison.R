#' @export
vec_proxy_equal.phinterval <- function(x, ...) {
  vec_data(x)
}

# TODO: I'm not sure what to do for this. Maybe just implement `<` directly?
# - Yes see bit64 implementation of the individual comparison operators:
#   https://github.com/r-lib/bit64/blob/main/R/integer64.R
#' @export
`<.phinterval` <- function(e1, e2) {
  check_is_phintish(e1)
  check_is_phintish(e2)
  check_recycleable(e1, e2)

  cpp_interval_sets_lt(vec_data(as_phinterval(e1)), vec_data(as_phinterval(e2)))
}

#' @export
`>.phinterval` <- function(e1, e2) {
  e2 < e1
}

#' @export
`<=.phinterval` <- function(e1, e2) {
  check_is_phintish(e1)
  check_is_phintish(e2)
  check_recycleable(e1, e2)

  cpp_interval_sets_leq(vec_data(as_phinterval(e1)), vec_data(as_phinterval(e2)))
}

#' @export
`>=.phinterval` <- function(e1, e2) {
  check_is_phintish(e1)
  check_is_phintish(e2)
  check_recycleable(e1, e2)

  cpp_interval_sets_geq(vec_data(as_phinterval(e1)), vec_data(as_phinterval(e2)))
}
