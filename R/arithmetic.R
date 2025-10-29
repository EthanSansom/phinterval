divide_phinterval_by_duration <- function(phint, dur) {
  as.duration(phint) / dur
}

divide_phinterval_by_difftime <- function(phint, diff) {
  as.duration(phint) / diff
}

divide_phinterval_by_period <- function(phint, per) {
  map2_dbl(phint_to_spans(phint), per, \(spans, p) sum(spans / p))
}

trunc_divide <- function(e1, e2) trunc(e1 / e2)

#' @export
#' @method vec_arith phinterval
vec_arith.phinterval <- function(op, x, y, ...) {
  UseMethod("vec_arith.phinterval", y)
}

#' @export
#' @method vec_arith.phinterval default
vec_arith.phinterval.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.phinterval Duration
vec_arith.phinterval.Duration <- function(op, x, y, ...) {
  switch(
    op,
    `/` = divide_phinterval_by_duration(x, y),
    `%/%` = trunc_divide(x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.phinterval Period
vec_arith.phinterval.Period <- function(op, x, y, ...) {
  switch(
    op,
    `/` = divide_phinterval_by_period(x, y),
    `%/%` = trunc_divide(x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.phinterval difftime
vec_arith.phinterval.difftime <- function(op, x, y, ...) {
  switch(
    op,
    `/` = divide_phinterval_by_difftime(x, y),
    `%/%` = trunc_divide(x, y),
    stop_incompatible_op(op, x, y)
  )
}
