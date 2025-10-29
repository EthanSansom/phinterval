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
  } else if (isTRUE(x_len %% y_len == 0)) {
    list(x = x, y = vctrs::vec_rep(y, x_len / y_len))
  } else if (isTRUE(y_len %% x_len == 0)) {
    list(x = vctrs::vec_rep(x, y_len / x_len), y = y)
  } else {
    cli::cli_abort(
      paste0(
        "Can't recycle {.arg {x_arg}} (size {x_len}) ",
        "to match {.arg {y_arg}} (size {y_len})."
      ),
      call = error_call,
      class = "phinterval_error_incompatible_length"
    )
  }
}

# TODO Ethan: What we really want is a `recycle_to(to, arg)` option, which
#             recycles the other arguments to the length of `to`. This allows
#             `left` and `right` to be recycled to the length of `phint` in the
#             function `phint_bound`.
recycle_to <- function(
    x,
    to,
    x_arg = rlang::caller_arg(x),
    to_arg = rlang::caller_arg(to),
    error_call = rlang::caller_env()
  ) {

  x_len <- length(x)
  to_len <- length(to)

  if (to_len == x_len) {
    x
  } else if (isTRUE(to_len %% x_len == 0)) {
    vctrs::vec_rep(x, to_len / x_len)
  } else {
    cli::cli_abort(
      paste0(
        "Can't recycle {.arg {x_arg}} (size {x_len}) ",
        "to match {.arg {to_arg}} (size {to_len})."
      ),
      call = error_call,
      class = "phinterval_error_incompatible_length"
    )
  }
}

commas <- function(x, sep = ",", sep2 = "", last = "or") {
  x_len <- length(x)
  if (x_len == 1) {
    return(x)
  }
  sep <- if (x_len == 2) sep2 else sep
  paste(c(paste0(x[-x_len], sep), last, x[[x_len]]), collapse = " ")
}

# time wrangling ---------------------------------------------------------------

# TODO Ethan:
# Make a separate `timezones.R` script for timezone stuff. Will include `with_tz`
# `force_tz`, and `tz` implementations for `phinterval`.

get_tzone <- function(x) {
  UseMethod("get_tzone")
}

get_tzone.Interval <- function(x) {
  lubridate::tz(lubridate::int_start(x))
}

get_tzone.phinterval <- function(x) {
  attr(x, "tzone")
}

get_tzone.default <- function(x) {
  lubridate::tz(x)
}

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
