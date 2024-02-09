# TODO Ethan:
# - make a different display method for tibbles! See https://vctrs.r-lib.org/articles/s3-vector.html#format-method
# - add documentation for public functions

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
      "{.var tzone} must be a scalar character vector"
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

vec_ptype_abbr.phinterval <- function(x, ...) {
  "phintrvl"
}

vec_ptype2.phinterval.phinterval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

vec_cast.phinterval.phinterval <- function(x, to, ...) x

vec_ptype2.phinterval.Interval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

vec_ptype2.Interval.phinterval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

vec_cast.phinterval.Interval <- function(x, to, ...) as_phinterval(x)

as_phinterval <- function(x, ...) {
  UseMethod("as_phinterval")
}

as_phinterval.default <- function(x, ...) {
  vctrs::vec_cast(x, new_phinterval())
}

# Consider each element of the `Interval` vector to be an element of the `phinterval`.
as_phinterval.Interval <- function(x, tzone = NULL) {

  if (!lubridate::is.interval(x)) {
    cli::cli_abort(
      paste0(
        "{.arg x} must be a {.cls {Interval}} vector, ",
        "not a {.cls {class(x)}."
      )
    )
  }

  # TODO: Make a consistent error messaging script (with error functions)
  tzone <- if (is.null(tzone)) "UTC" else tzone
  stopifnot(rlang::is_scalar_character(tzone))

  intvl <- lubridate::int_standardize(x)
  new_phinterval(
    reference_time = lubridate::int_start(intvl),
    range_starts = as.list(rep(0, length(intvl))),
    range_ends = as.list(intvl_length_dbl(intvl)),
    tzone = tzone
  )

}


# Initialize a phinterval from a list interval vectors
phinterval <- function(intervals = NULL, tzone = NULL) {

  if (is.null(intervals) && is.null(tzone)) {
    return(new_phinterval())
  } else if (is.null(intervals)) {
    stop_wrong_type(tzone, "character")
    return(
      new_phinterval(
        reference_time = POSIXct(0L, tz = tzone),
        range_starts = list(),
        range_ends = list(),
        tzone = tzone
      )
    )
  } else if (is.null(tzone)) {
    tzone <- "UTC"
  }

  # TODO Fix this error messaging (you have not implemented `stop_wrong_type`),
  # and decide what to do when user provides an interval vector rather than a
  # list of interval vectors.
  stop_wrong_type(tzone, "character", n = 1)
  if (lubridate::is.interval(intervals)) {
    return(as_phinterval(intervals, tzone))
  }
  if (!is.list(intervals)) {
    cli::cli_abort(
      paste0(
        "{.arg intervals} must be a list of {.cls Interval} vectors, ",
        "not a {.cls {class()}."
      )
    )
  }

  # Standardizing the interval ensures that the `phinterval` ranges are positive.
  intervals <- map(intervals, lubridate::int_standardize)

  # Implicitly, `reference_time[[i]]` will be NA wherever any element of interval
  # `intervals[[i]]` is NA.
  min_start_time <- map_dbl(intervals, \(ivl) min(intvl_start_dbl(ivl)))
  reference_time <- min_start_time |>
    lubridate::as_datetime() |>
    lubridate::with_tz(tzone)

  range_starts <- map2(
    intervals,
    min_start_time,
    \(ivl, min_start) { intvl_start_dbl(ivl) - min_start }
  )
  range_ends <- map2(
    intervals,
    min_start_time,
    \(ivl, min_start) { intvl_end_dbl(ivl) - min_start }
  )

  range_starts[is.na(reference_time)] <- NA_real_
  range_ends[is.na(reference_time)] <- NA_real_
  ranges <- flatten_overlapping_ranges(range_starts, range_ends)

  new_phinterval(
    reference_time = reference_time,
    range_starts = ranges$starts,
    range_ends = ranges$ends,
    tzone = tzone
  )

}

flatten_overlapping_ranges <- function(range_starts, range_ends) {

  contains_overlaps <- map2_lgl(range_starts, range_ends, range_contains_overlaps)
  contains_overlaps[is.na(contains_overlaps)] <- FALSE

  if (any(contains_overlaps)) {
    range_unions <- map2(
      range_starts[contains_overlaps],
      range_ends[contains_overlaps],
      range_flatten
    )
    range_starts[contains_overlaps] <- map(range_unions, `[[`, "starts")
    range_ends[contains_overlaps] <- map(range_unions, `[[`, "ends")
  }

  list(starts = range_starts, ends = range_ends)

}

# `phinterval`s are equal when they represent the same collect of time-spans,
# ignoring timezone. This is unlike `Interval` equality, which is determined by
# duration of the time-span.
# https://github.com/tidyverse/lubridate/issues/1135
vec_proxy_equal.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")

  data.frame(
    reference_time = as.double(reference_time),
    range_starts = map_chr(range_starts, \(x) paste0(sort(x), collapse = "")),
    range_ends = map_chr(range_ends, \(x) paste0(sort(x), collapse = ""))
  )

}

# `phinterval`s are ordered by start, end, and duration.
vec_proxy_compare.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")

  min_start <- map2_dbl(reference_time, range_starts, \(t, s) t + min(s))
  max_end <- map2_dbl(reference_time, range_ends, \(t, e) t + max(e))
  duration_seconds <- map2(range_starts, range_ends, \(s, e) sum(e - s))

  data.frame(
    min_start = min_start,
    max_end = max_end,
    duration_seconds = duration_seconds
  )

}

vec_arith.phinterval <- function(op, x, y, ...) {
  UseMethod("vec_arith.phinterval", y)
}

vec_arith.phinterval.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}
