new_phinterval <- function(
    reference_time = lubridate::POSIXct(tz = "UTC"),
    range_starts = list(),
    range_ends = list(),
    tzone = attr(reference_time, "tzone")
) {

  bullets <- character()
  if (!lubridate::is.POSIXct(reference_time)) {
    bullets <- c(
      bullets,
      paste0(
        "{.var reference_time} must be a {.cls {POSIXct}}, ",
        "not a {.cls {class(reference_time)}"
      )
    )
  }
  if (!is_list_of_dbl(range_starts)) {
    bullets <- c(
      bullets,
      "{.var range_starts} must be a list of double vectors"
    )
  }
  if (!is_list_of_dbl(range_ends)) {
    bullets <- c(
      bullets,
      "{.var range_ends} must be a list of double vectors"
    )
  }
  if (!rlang::is_scalar_character(tzone)) {
    bullets <- c(
      bullets,
      "{.var tzone} must be a length-1 character vector"
    )
  }
  if (!rlang::is_empty(bullets)) {
    cli::cli_abort(
      c(
        "Attempted to create malformed {.cls phinterval}.",
        setNames(bullets, rep("x", length(bullets)))
      )
    )
  }

  vctrs::new_rcrd(
    fields = list(
      reference_time = reference_time,
      range_starts = range_starts,
      range_ends = range_ends
    ),
    tzone = tzone,
    class = "phinterval"
  )

}

# Following `format.Interval` closely from: https://github.com/tidyverse/lubridate/blob/main/R/intervals.r

#' @export
format.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")
  tzone <- attr(x, "tzone")

  starts_order <- map(range_starts, order)
  interval_starts <-
    map2(reference_time, range_starts, `+`) |>
    map2(starts_order, `[`) |>
    map(format, usetz = FALSE)
  interval_ends <-
    map2(reference_time, range_ends, `+`) |>
    map2(starts_order, `[`) |>
    map(format, usetz = FALSE)

  out <- paste0(
    "[",
    map2_chr(
      interval_starts,
      interval_ends,
      \(x, y) paste(x, y, sep = "--", collapse = ", ")
    ),
    "] ",
    tzone
  )
  out[is.na(reference_time)] <- NA_character_
  out

}

# This is used at the class abbreviation in tibbles and `str()`
vec_ptype_abbr.phinterval <- function(x, ...) {
  "phintrvl"
}

# TODO Ethan: This class definitely needs a different display method for tibbles!
#             See https://vctrs.r-lib.org/articles/s3-vector.html#format-method


vec_ptype2.phinterval.phinterval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

vec_cast.phinterval.phinterval <- function(x, to, ...) x

vec_ptype2.phinterval.Interval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

vec_ptype2.Interval.phinterval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

vec_cast.phinterval.Interval <- function(x, to, ...) as_phinterval(x)

# Implementing this as a generic (instead of relying on `vec_cast` methods alone)
# for some flexibility to cast an Interval into a `phinterval`.
as_phinterval <- function(x, ...) {
  UseMethod("as_phinterval")
}

# This will only work when `x` is a `phinterval` (since we've only defined that
# `vec_cast` method), but `vec_cast` will provide an informative error message for
# free.
as_phinterval.default <- function(x, ...) {
  vctrs::vec_cast(x, new_phinterval())
}

# Consider each element of the `Interval` vector to be an element of the `phinterval`.
as_phinterval.Interval <- function(x, tzone = NULL) {

  tzone <- if (is.null(tzone)) "UTC" else tzone

  # TODO: Make a consistent error messaging script (with error functions)
  stopifnot(rlang::is_scalar_character(tzone))

  if (!lubridate::is.interval(x)) {
    cli::cli_abort(
      paste0(
        "{.arg x} must be a {.cls {Interval}} vector, ",
        "not a {.cls {class(x)}."
      )
    )
  }

  ivl <- lubridate::int_standardize(x)
  new_phinterval(
    reference_time = lubridate::int_start(ivl),
    range_starts = as.list(rep(0, length(ivl))),
    range_ends = as.list(as.double(lubridate::int_length(ivl))),
    tzone = tzone
  )

}
