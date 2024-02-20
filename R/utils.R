# misc -------------------------------------------------------------------------
`%notin%` <- Negate(`%in%`)

pmap_lgl <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "logical")
}

# Implements common `base` recycling rules (failing if impossible). See
# https://vctrs.r-lib.org/articles/type-size.html#appendix-recycling-in-base-r
# for `base` recycling rules vs. `vctrs` recycling rules.
recycle2_common <- function(
    x,
    y,
    x_arg = rlang::caller_arg(x),
    y_arg = rlang::caller_arg(y),
    error_call = rlang::caller_env()
  ) {

  x_len <- length(x)
  y_len <- length(y)

  if (x_len == y_len) {
    list(x = x, y = y)
  } else if (x_len == 1) {
    list(x = vctrs::vec_rep(x, y_len), y = y)
  } else if (y_len == 1) {
    list(x = x, y = vctrs::vec_rep(y, y_len))
  } else if (isTRUE(x_len %% y_len == 0)) {
    list(x = x, y = vctrs::vec_rep(y, x_len / y_len))
  } else if (isTRUE(y_len %% x_len == 0)) {
    list(x = vctrs::vec_rep(x, y_len / x_len), y = y)
  } else {
    cli::cli_abort(
      "Can't recycle {.arg {x_arg}} (size {x_len}) to match {.arg {y_arg}} (size {y_len}).",
      call = error_call
    )
  }

}

# predicates -------------------------------------------------------------------

# TODO Ethan: I don't like any of these predicates, find and replace them.

is_list_of_dbl <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, \(x) rlang::is_double(x) || all(is.na(x))))
}

is_list_of_POSIXct <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, \(x) lubridate::is.POSIXct(x) || all(is.na(x))))
}

is_list_of_Interval <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, \(x) lubridate::is.interval(x) || all(is.na(x))))
}

# time wrangling ---------------------------------------------------------------

# Both `tz_is_local` and `tz_union` are borrowed directly from lubridate for
# consistency. https://github.com/tidyverse/lubridate/blob/main/R/vctrs.R
tz_is_local <- function(x) {
  identical(x, "")
}

tz_union <- function(x, y) {
  x_tzone <- attr(x, "tzone")
  y_tzone <- attr(y, "tzone")

  if (tz_is_local(x_tzone)) {
    y_tzone
  } else {
    x_tzone
  }
}

# interval helpers -------------------------------------------------------------

# TODO Ethan: I don't like any of these helpers, find and replace them.

intvl_start_dbl  <- function(intvl) as.double(lubridate::int_start(intvl))
intvl_end_dbl    <- function(intvl) as.double(lubridate::int_end(intvl))
intvl_length_dbl <- function(intvl) as.double(lubridate::int_length(intvl))
