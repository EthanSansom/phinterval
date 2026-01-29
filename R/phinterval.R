# todos ------------------------------------------------------------------------

# TODO: Add a `bounds = c("inclusive", "[]", "exclusive", "()")` argument to:
# - `phint_overlaps()`
# - `phint_intersect()`

# TODO: Documentation
# - Update README for new interface / functionality
# - Create a phinterval and dplyr vignette, show speed of `by` vs. `group_by()`
#
# TODO: CRAN
# - Look into all of the edge-case CRAN checks and run (see RPackages book)

# TODO: JOSS
# - Review paper and update to use new interface!

# constructors -----------------------------------------------------------------

setOldClass(c("phinterval", "list", "vctrs_rcrd"))

#' Create a new phinterval
#'
#' @description
#'
#' `phinterval()` creates a new `<phinterval>` vector from start and end times.
#' A phinterval (think "potentially holey interval") is a span of time which may
#' contain gaps.
#'
#' @details
#'
#' The `<phinterval>` class is designed as a generalization of the
#' [lubridate::interval()]. While an `<Interval>` element represents a
#' single contiguous span between two fixed times, a `<phinterval>` element can
#' represent a time span that may be empty, contiguous, or disjoint (i.e. containing
#' gaps). Each element of a `<phinterval>` is stored as a (possibly empty) set of
#' non-overlapping and non-abutting time spans.
#'
#' When `by = NULL` (the default), `phinterval()` creates scalar phinterval
#' elements, where each element contains a single time span from `start[i]` to
#' `end[i]`. This is equivilant to [lubridate::interval()]:
#'
#' ```
#' interval(start, end, tzone = tzone)   # <Interval> vector
#' phinterval(start, end, tzone = tzone) # <phinterval> vector
#' ```
#'
#' When `by` is provided, `phinterval()` groups the `start`/`end` pairs by the
#' values in `by`, creating phinterval elements that may contain multiple disjoint
#' time spans. Overlapping or abutting spans within each group are automatically
#' merged.
#'
#' @param start `[POSIXct / POSIXlt / Date]`
#'
#' A vector of start times. Must be recyclable with `end`.
#'
#' @param end `[POSIXct / POSIXlt / Date]`
#'
#' A vector of end times. Must be recyclable with `start`.
#'
#' @param tzone `[character(1)]`
#'
#' A time zone to display the `<phinterval>` in. If `tzone` is `NULL`
#' (the default), then the time zone is taken from that of `start`.
#'
#' `tzone` can be any non-`NA` string, but unrecognized time zones (see
#' [is_recognized_tzone()]) will be formatted using `"UTC"` with a warning.
#'
#' @param by `[vector / data.frame / NULL]`
#'
#' An optional grouping vector or data frame. When provided, `start[i]` and
#' `end[i]` pairs are grouped by `by[i]`, creating one phinterval element per
#' unique value of `by`. Overlapping or abutting spans within each group are
#' merged. If `NULL` (the default), each `start`/`end` pair creates a separate
#' phinterval element. `by` is recycled to match the common length of `start`
#' and `end`.
#'
#' `by` must be a vector in the vctrs sense. See `[vctrs::obj_is_vector()]`
#' for details.
#'
#' @param order_by `[TRUE / FALSE]`
#'
#' Should the output be ordered by the values in `by`? If `FALSE` (the default),
#' the output order matches the first appearance of each group in `by`. If `TRUE`,
#' the output is sorted by the unique values of `by`. Only used when `by` is not
#' `NULL`.
#'
#' @return
#'
#' When `by = NULL`, a `<phinterval>` vector the same length as the recycled
#' length of `start` and `end`.
#'
#' When `by` is provided, a `<phinterval>` vector with one element per unique
#' value of `by`.
#'
#' @examples
#' # Scalar phintervals (equivalent to interval())
#' phinterval(
#'   start = as.Date(c("2000-01-01", "2000-02-01")),
#'   end = as.Date(c("2000-02-01", "2000-03-01"))
#' )
#'
#' # Grouped phintervals with multiple spans per element
#' phinterval(
#'   start = as.Date(c("2000-01-01", "2000-03-01", "2000-02-01")),
#'   end = as.Date(c("2000-02-01", "2000-04-01", "2000-03-01")),
#'   by = c(1, 1, 2)
#' )
#'
#' # Overlapping spans are merged within groups
#' phinterval(
#'   start = as.Date(c("2000-01-01", "2000-01-15")),
#'   end = as.Date(c("2000-02-01", "2000-02-15")),
#'   by = 1
#' )
#'
#' # Empty phinterval
#' phinterval()
#'
#' # Specify time zone
#' phinterval(
#'   start = as.Date("2000-01-01"),
#'   end = as.Date("2000-02-01"),
#'   tzone = "America/New_York"
#' )
#'
#' @export
phinterval <- function(
    start = POSIXct(),
    end = POSIXct(),
    tzone = NULL,
    by = NULL,
    order_by = FALSE
) {
  check_instant(start)
  check_instant(end)
  check_recycleable(start, end)
  check_string(tzone, allow_null = TRUE)
  check_vector(by, allow_null = TRUE)
  check_bool(order_by)

  tzone <- tzone %||% tz_union(start, end)
  range <- vec_recycle_common(starts = as.POSIXct(start), ends = as.POSIXct(end))

  if (is_null(by)) {
    out <- as_phint_range_cpp(starts = range$starts, ends = range$ends)
    return(new_phinterval_bare(out, tzone = tzone))
  }

  check_recycleable_to(
    x = by,
    to = range$starts,
    to_arg = "vctrs::vec_recycle_common(start, end)"
  )

  datetime_squash_impl(
    starts = range$starts,
    ends = range$ends,
    by = by,
    tzone = tzone,
    na.rm = FALSE,
    empty_to = "empty",
    order_by = order_by,
    keep_by = FALSE
  )
}

#' Create a hole phinterval
#'
#' @description
#'
#' `hole()` creates a `<phinterval>` vector where each element is a hole (an
#' empty set of time spans).
#'
#' @details
#'
#' A hole is a phinterval element with zero time spans, representing an empty
#' interval. Holes are useful as placeholders or for representing the absence
#' of time periods in interval algebra operations.
#'
#' @param n `[integerish(1)]`
#'
#' The number of hole elements to create. Must be a positive whole number.
#'
#' @param tzone `[character(1)]`
#'
#' A time zone to display the `<phinterval>` in. Defaults to `""`.
#'
#' @return
#'
#' A `<phinterval>` vector of length `n` where each element is a `<hole>`.
#'
#' @examples
#' # Create a single hole
#' hole()
#'
#' # Create multiple holes
#' hole(3)
#'
#' # Specify time zone
#' hole(tzone = "UTC")
#'
#' # Holes can be combined with other phintervals
#' jan <- phinterval(as.Date("2000-01-01"), as.Date("2000-02-01"))
#' c(jan, hole(), jan)
#'
#' @export
hole <- function(n = 1L, tzone = "") {
  check_number_whole(n, min = 1)
  check_string(tzone)

  points <- rep(list(numeric()), n)
  new_phinterval(
    size = rep(0L, n),
    starts = points,
    ends = points,
    tzone = tzone
  )
}

na_phinterval <- function(tzone) {
  new_phinterval(
    size = NA_integer_,
    starts = list(NULL),
    ends = list(NULL),
    tzone = tzone
  )
}

new_phinterval <- function(
    size = integer(),
    starts = list(),
    ends = list(),
    tzone = "UTC"
) {
  check_string(tzone, call = caller_env())
  new_rcrd(
    fields = list(
      size = size,
      starts = starts,
      ends = ends
    ),
    tzone = tzone,
    class = "phinterval"
  )
}

# Exported Rcpp functions return the `fields` directly
new_phinterval_bare <- function(fields, tzone = "UTC") {
  check_string(tzone, call = caller_env())
  new_rcrd(
    fields = fields,
    tzone = tzone,
    class = "phinterval"
  )
}

# vctrs ------------------------------------------------------------------------

#' @export
vec_proxy_equal.phinterval <- function(x, ...) {
  vec_data(x)
}

#' @export
vec_ptype2.phinterval.phinterval <- function(x, y, ...) {
  new_phinterval(tzone = tz_union(x, y))
}

#' @export
vec_ptype2.phinterval.Interval <- function(x, y, ...) {
  new_phinterval(tzone = tz_union(x, y))
}

#' @export
vec_ptype2.Interval.phinterval <- function(x, y, ...) {
  new_phinterval(tzone = tz_union(x, y))
}

#' @export
vec_cast.phinterval.phinterval <- function(x, to, ...) {
  x_tzone <- get_tzone(x)
  to_tzone <- get_tzone(to)

  if (identical(x_tzone, to_tzone)) {
    return(x)
  }

  attr(x, "tzone") <- to_tzone
  x
}

#' @export
vec_cast.phinterval.Interval <- function(x, to, ...) {
  x_tzone <- get_tzone(x)
  to_tzone <- get_tzone(to)

  if (identical(x_tzone, to_tzone)) {
    return(as_phinterval(x))
  }

  out <- as_phinterval(x)
  attr(out, "tzone") <- to_tzone
  out
}

#' @export
is.na.phinterval <- function(x) {
  is.na(field(x, "size"))
}

#' @export
anyNA.phinterval <- function(x, recursive = FALSE) {
  anyNA(field(x, "size"))
}

#' Test if the object is a phinterval
#'
#' @description
#'
#' This function returns `TRUE` for `<phinterval>` vectors and returns
#' `FALSE` otherwise.
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a `<phinterval>`, `FALSE` otherwise.
#'
#' @examples
#' is_phinterval(phinterval())
#' is_phinterval(interval())
#'
#' @export
is_phinterval <- function(x) {
  inherits(x, "phinterval")
}

#' Test if the object is a phinterval or interval
#'
#' @description
#'
#' This function returns `TRUE` for `<phinterval>` and `<Interval>` vectors and
#' returns `FALSE` otherwise.
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is a `<phinterval>` or `<Interval>`, `FALSE` otherwise.
#'
#' @examples
#' is_phinterval(phinterval())
#' is_phinterval(interval())
#' is_phinterval(as.Date("2020-01-01"))
#'
#' @export
is_phintish <- function(x) {
  is_phinterval(x) || lubridate::is.interval(x)
}

#' Test if the object is a recognized time zone
#'
#' @description
#'
#' `is_recognized_tzone()` returns `TRUE` for strings that are recognized IANA
#' time zone names, and `FALSE` otherwise.
#'
#' @details
#'
#' Recognized time zones are those listed in [tzdb::tzdb_names()], which
#' provides an up-to-date copy of time zones from the IANA time zone database.
#'
#' `<phinterval>` vectors with an unrecognized time zone are formatted using
#' the `"UTC"` time zone with a warning.
#'
#' @param x An object to test.
#'
#' @return `TRUE` if `x` is a recognized time zone, `FALSE` otherwise.
#'
#' @examples
#' is_recognized_tzone("UTC")
#' is_recognized_tzone("America/New_York")
#' is_recognized_tzone("")
#' is_recognized_tzone("badzone")
#' is_recognized_tzone(10L)
#'
#' @export
is_recognized_tzone <- function(x) {
  is_string(x) && tzone_is_valid_cpp(x)
}

#' Convert an interval or datetime vector into a phinterval
#'
#' @description
#'
#' `as_phinterval()` converts a [lubridate::interval()], Date, POSIXct, or POSIXlt
#' vector into an equivalent `<phinterval>` vector.
#'
#' @details
#'
#' Negative intervals (where start > end) are standardized to positive intervals
#' via [lubridate::int_standardize()].
#'
#' Datetime vectors (Date, POSIXct, POSIXlt) are converted into instantaneous
#' intervals where the start and end are identical.
#'
#' Spans with partially missing endpoints (e.g., `interval(NA, end)` or
#' `interval(start, NA)`) are converted to a fully `NA` element.
#'
#' @param x `[Interval / Date / POSIXct / POSIXlt]`
#'
#' An object to convert.
#'
#' @param ... Additional arguments passed to methods. Currently unused.
#'
#' @return A `<phinterval>` vector the same length as `x`.
#'
#' @seealso [phinterval()]
#'
#' @examples
#' # Convert Interval vector
#' years <- interval(
#'   start = as.Date(c("2021-01-01", "2023-01-01")),
#'   end = as.Date(c("2022-01-01", "2024-01-01"))
#' )
#' as_phinterval(years)
#'
#' # Negative intervals are standardized
#' negative <- interval(as.Date("2000-10-11"), as.Date("2000-10-01"))
#' as_phinterval(negative)
#'
#' # Partially missing endpoints become fully NA
#' partial_na <- interval(NA, as.Date("1999-08-02"))
#' as_phinterval(partial_na)
#'
#' # Datetime vectors become instantaneous intervals
#' as_phinterval(as.Date(c("2000-10-11", "2001-05-03")))
#'
#' @export
as_phinterval <- function(x, ...) {
  UseMethod("as_phinterval")
}

#' @rdname as_phinterval
#' @export
as_phinterval.default <- function(x, ...) {
  if (is_instant(x)) {
    new_phinterval_bare(
      fields = as_phint_point_cpp(as.POSIXct(x)),
      tzone = get_tzone(x)
    )
  } else {
    vec_cast(x, new_phinterval())
  }
}

#' @rdname as_phinterval
#' @export
as_phinterval.Interval <- function(x, ...) {
  new_phinterval_bare(
    fields = as_phint_intvl_cpp(
      starts = lubridate::int_start(x),
      spans = lubridate::int_length(x)
    ),
    tzone = get_tzone(x)
  )
}

# arithmetic -------------------------------------------------------------------

#' @export
#' @method vec_arith phinterval
vec_arith.phinterval <- function(op, x, y, ...) {
  UseMethod("vec_arith.phinterval", y)
}

#' @export
#' @method vec_arith.phinterval Duration
vec_arith.phinterval.Duration <- function(op, x, y, ...) {
  switch(
    op,
    "/" = as_duration(x) / y,
    "%/%" = trunc(as_duration(x) / y),
    stop_incompatible_op(op, x, y)
  )
}

# accessors --------------------------------------------------------------------

#' Count the number of spans in a phinterval
#'
#' @description
#'
#' `n_spans()` counts the number of disjoint time spans in each element of
#' `phint`.
#'
#' @inheritParams params
#'
#' @return An integer vector the same length as `phint`.
#'
#' @examples
#' # Count spans
#' y2000 <- interval(as.Date("2000-01-01"), as.Date("2001-01-01"))
#' y2025 <- interval(as.Date("2025-01-01"), as.Date("2025-01-01"))
#'
#' n_spans(c(
#'  phint_union(y2000, y2025),
#'  phint_intersect(y2000, y2025),
#'  y2000, y2025
#' ))
#'
#' @export
n_spans <- function(phint) {
  UseMethod("n_spans")
}

#' @rdname n_spans
#' @export
n_spans.default <- function(phint) {
  check_phintish(phint)
}

#' @rdname n_spans
#' @export
n_spans.Interval <- function(phint) {
  out <- rep(1L, length(phint))
  out[is.na(phint)] <- NA_integer_
  out
}

#' @rdname n_spans
#' @export
n_spans.phinterval <- function(phint) {
  field(phint, "size")
}

#' Test for empty intervals
#'
#' @description
#'
#' `is_hole()` checks for `<hole>` (empty) time spans in `phint`.
#'
#' @inheritParams params
#'
#' @return A logical vector the same length as `phint`.
#'
#' @examples
#' # Detect holes
#' y2000 <- interval(as.Date("2000-01-01"), as.Date("2001-01-01"))
#' y2025 <- interval(as.Date("2025-01-01"), as.Date("2025-01-01"))
#' is_hole(c(hole(), y2000, hole(), y2025, NA))
#'
#' # The intersection of disjoint intervals is a hole
#' is_hole(phint_intersect(y2000, y2025))
#'
#' @export
is_hole <- function(phint) {
  n_spans(phint) == 0L
}

#' Accessors for the endpoints of a phinterval
#'
#' @description
#'
#' `phint_start()` and `phint_end()` return the earliest and latest endpoint
#' of each phinterval element, respectively. Holes (empty time spans) are
#' returned as `NA`.
#'
#' `phint_starts()` and `phint_ends()` return lists of all start and end points
#' for each phinterval element, respectively. For phintervals with multiple
#' disjoint spans, each span's endpoint is included. Holes are returned as
#' length-0 `<POSIXct>` vectors.
#'
#' @inheritParams params
#'
#' @return
#'
#' For `phint_start()` and `phint_end()`, a `<POSIXct>` vector the same length
#' as `phint`.
#'
#' For `phint_starts()` and `phint_ends()`, a list of `<POSIXct>` vectors the
#' same length as `phint`.
#'
#' @name phinterval-accessors
#'
#' @examples
#' int1 <- interval(as.Date("2020-01-10"), as.Date("2020-02-01"))
#' int2 <- interval(as.Date("2023-05-02"), as.Date("2023-06-03"))
#'
#' phint_start(int1)
#' phint_end(int1)
#'
#' # Holes have no endpoints; disjoint phintervals have multiple endpoints
#' hole <- phint_intersect(int1, int2)
#' disjoint <- phint_union(int1, int2)
#'
#' phint_start(c(hole, disjoint))
#' phint_starts(c(hole, disjoint))
#'
#' phint_end(c(hole, disjoint))
#' phint_ends(c(hole, disjoint))
#'
#' # phint_start() and phint_end() return the minimum and maximum endpoints
#' negative <- interval(as.Date("1980-01-01"), as.Date("1979-12-27"))
#' phint_start(negative)
#' phint_end(negative)
NULL

#' @rdname phinterval-accessors
#' @export
phint_start <- function(phint) {
  UseMethod("phint_start")
}

#' @rdname phinterval-accessors
#' @export
phint_start.default <- function(phint) {
  check_phintish(phint)
}

#' @rdname phinterval-accessors
#' @export
phint_start.Interval <- function(phint) {
  intvl_start_cpp(
    starts = lubridate::int_start(phint),
    spans = lubridate::int_length(phint),
    tzone = get_tzone(phint)
  )
}

#' @rdname phinterval-accessors
#' @export
phint_start.phinterval <- function(phint) {
  phint_start_cpp(
    size = field(phint, "size"),
    starts = field(phint, "starts"),
    tzone = get_tzone(phint)
  )
}

#' @rdname phinterval-accessors
#' @export
phint_end <- function(phint) {
  UseMethod("phint_end")
}

#' @rdname phinterval-accessors
#' @export
phint_end.default <- function(phint) {
  check_phintish(phint)
}

#' @rdname phinterval-accessors
#' @export
phint_end.Interval <- function(phint) {
  intvl_end_cpp(
    starts = lubridate::int_start(phint),
    spans = lubridate::int_length(phint),
    tzone = get_tzone(phint)
  )
}

#' @rdname phinterval-accessors
#' @export
phint_end.phinterval <- function(phint) {
  phint_end_cpp(
    size = field(phint, "size"),
    ends = field(phint, "ends"),
    tzone = get_tzone(phint)
  )
}

#' @rdname phinterval-accessors
#' @export
phint_starts <- function(phint) {
  UseMethod("phint_starts")
}

#' @rdname phinterval-accessors
#' @export
phint_starts.default <- function(phint) {
  check_phintish(phint)
}

#' @rdname phinterval-accessors
#' @export
phint_starts.Interval <- function(phint) {
  as.list(phint_start(phint))
}

#' @rdname phinterval-accessors
#' @export
phint_starts.phinterval <- function(phint) {
  phint_points_cpp(
    size = field(phint, "size"),
    points = field(phint, "starts"),
    tzone = get_tzone(phint)
  )
}

#' @rdname phinterval-accessors
#' @export
phint_ends <- function(phint) {
  UseMethod("phint_ends")
}

#' @rdname phinterval-accessors
#' @export
phint_ends.default <- function(phint) {
  check_phintish(phint)
}

#' @rdname phinterval-accessors
#' @export
phint_ends.Interval <- function(phint) {
  as.list(phint_end(phint))
}

#' @rdname phinterval-accessors
#' @export
phint_ends.phinterval <- function(phint) {
  phint_points_cpp(
    size = field(phint, "size"),
    points = field(phint, "ends"),
    tzone = get_tzone(phint)
  )
}

#' Compute the length of a phinterval in seconds
#'
#' @description
#'
#' `phint_length()` calculates the total length of all time spans within each
#' phinterval element in seconds. For phintervals with multiple disjoint spans,
#' the lengths are summed. Instantaneous intervals and holes have length 0.
#'
#' `phint_lengths()` returns the individual length in seconds of each time span
#' within each phinterval element.
#'
#' @inheritParams params
#'
#' @return
#'
#' For `phint_length()`, a numeric vector the same length as `phint`.
#'
#' For `phint_lengths()`, a list of numeric vectors the same length as `phint`.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#'
#' phint_length(monday)
#' phint_length(phint_intersect(monday, friday))
#'
#' # phint_length() sums the lengths of disjoint time spans
#' mon_and_fri <- phint_union(monday, friday)
#' phint_length(mon_and_fri) == phint_length(monday) + phint_length(friday)
#'
#' # phint_lengths() returns the length of each disjoint time span
#' phint_lengths(mon_and_fri)
#'
#' @export
phint_length <- function(phint) {
  UseMethod("phint_length")
}

#' @rdname phint_length
#' @export
phint_length.default <- function(phint) {
  check_phintish(phint)
}

#' @rdname phint_length
#' @export
phint_length.Interval <- function(phint) {
  abs(lubridate::int_length(phint)) # <Interval> length can be negative
}

#' @rdname phint_length
#' @export
phint_length.phinterval <- function(phint) {
  phint_length_cpp(
    size = field(phint, "size"),
    starts = field(phint, "starts"),
    ends = field(phint, "ends")
  )
}

#' @rdname phint_length
#' @export
phint_lengths <- function(phint) {
  UseMethod("phint_lengths")
}

#' @rdname phint_length
#' @export
phint_lengths.default <- function(phint) {
  check_phintish(phint)
}

#' @rdname phint_length
#' @export
phint_lengths.Interval <- function(phint) {
  as.list(phint_length(phint))
}

#' @rdname phint_length
#' @export
phint_lengths.phinterval <- function(phint) {
  phint_lengths_cpp(
    size = field(phint, "size"),
    starts = field(phint, "starts"),
    ends = field(phint, "ends")
  )
}

#' Convert a phinterval to a duration
#'
#' @description
#'
#' `as_duration()` converts a [lubridate::interval()] or [phinterval()] vector
#' into a [lubridate::duration()] vector. The resulting duration measures the
#' length of time in seconds within each element of the interval or phinterval.
#'
#' `as_duration()` is a wrapper around [lubridate::as.duration()].
#'
#' @param x `[phinterval / Interval]`
#'
#' An object to convert.
#'
#' @param ... Parameters passed to other methods. Currently unused.
#'
#' @return A `<Duration>` vector the same length as `x`.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#' mon_and_fri <- phint_union(monday, friday)
#'
#' as_duration(c(mon_and_fri, monday))
#' as_duration(mon_and_fri) == as_duration(monday) + as_duration(friday)
#'
#' @export
as_duration <- function(x, ...) {
  UseMethod("as_duration")
}

#' @rdname as_duration
#' @export
as_duration.default <- function(x, ...) {
  lubridate::as.duration(x)
}

#' @rdname as_duration
#' @export
as_duration.phinterval <- function(x, ...) {
  lubridate::as.duration(phint_length(x))
}
