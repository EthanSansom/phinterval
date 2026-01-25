# todos ------------------------------------------------------------------------

# TODO: Revise unit tests to use the new structure!

# TODO: New unit testing categories
# - Check that IntvlVector/RangeVector deal with infinite starts, ends, and spans
# - Check the `na_view()` from `SetView()` and `SpanView()`

# TODO: New documentation
# - Remove all references to `phint_to_spans()`
# - Show speed of phint_squash(by) vs. group_by(by) |> mutate(phint_squash)
# - Document lubridate quirks that come with phinterval (e.g. setdiff with instants)
# - Explicitly document full treatment of timezones!

# constructors -----------------------------------------------------------------

setOldClass(c("phinterval", "list", "vctrs_rcrd"))

# TODO: Documentation
#' @export
phinterval <- function(start = POSIXct(), end = POSIXct(), tzone = NULL, by = NULL) {
  check_instant(start)
  check_instant(end)
  check_recycleable(start, end)
  check_string(tzone, allow_null = TRUE)

  tzone <- tzone %||% tz_union(start, end)
  range <- vec_recycle_common(starts = as.POSIXct(start), ends = as.POSIXct(end))
  if (!is_null(by)) {
    check_vector(by)
    check_recycleable_to(
      x = by,
      to = range$starts,
      to_arg = "vctrs::vec_recycle_common(start, end)"
    )
  }

  if (is.null(by)) {
    out <- as_phint_range_cpp(
      starts = range$starts,
      ends = range$ends
    )
  } else if (vec_size(by) == 1L) {
    # Equivalent to recycling `by`, then using `range_squash_by_cpp()`
    out <- range_squash_cpp(
      starts = range$starts,
      ends = range$ends,
      na_rm = TRUE
    )
  } else {
    groups <- vec_group_loc(by)
    out <- range_squash_by_cpp(
      starts = range$starts,
      ends = range$ends,
      group_locs = groups[["loc"]],
      na_rm = TRUE
    )
  }

  new_phinterval_bare(out, tzone = tzone)
}

# TODO: Document
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
vec_cast.phinterval.phinterval <- function(x, to, ...) x

#' @export
vec_cast.phinterval.Interval <- function(x, to, ...) as_phinterval(x)

#' @export
is.na.phinterval <- function(x) {
  is.na(field(x, "size"))
}

#' @export
anyNA.phinterval <- function(x) {
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

# TODO: Document
#' @export
is_valid_tzone <- function(x) {
  is_string(x) && tzone_is_valid_cpp(x)
}

# TODO: Document the instant case!
#' Convert an interval vector into a phinterval
#'
#' @description
#'
#' `as_phinterval()` changes a [lubridate::interval()] vector into the equivalent
#' `<phinterval>` vector. Negative intervals are flipped to positive (i.e.
#' via [lubridate::int_standardize()]).
#'
#' @param x An object to convert.
#' @param ... Parameters passed to other methods. Currently unused.
#' @return A `<phinterval>` vector the same length as `x`.
#'
#' @seealso [phinterval()]
#'
#' @examples
#' years <- interval(
#'   start = as.Date(c("2021-01-01", "2023-01-01")),
#'   end =  as.Date(c("2022-01-01", "2024-01-01"))
#' )
#' as_phinterval(years)
#'
#' # Negative intervals are standardized
#' (negative <- interval(as.Date("2000-10-11"), as.Date("2000-10-11") - 10))
#' as_phinterval(negative)
#'
#' # A <phinterval> cannot have partially missing endpoints
#' (partial_na <- interval(NA, as.Date("1999-08-02")))
#' as_phinterval(partial_na)
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
#' @return An integer vector the same length as `phint`.
#'
#' @examples
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
#' `is_hole()` checks for `<hole>` (i.e. empty) time spans in `phint`.
#'
#' @inheritParams params
#' @return A logical vector the same length as `phint`.
#'
#' @examples
#' y2000 <- interval(as.Date("2000-01-01"), as.Date("2001-01-01"))
#' y2025 <- interval(as.Date("2025-01-01"), as.Date("2025-01-01"))
#'
#' # The intersection of disjoint intervals is a hole
#' is_hole(c(
#'  phint_intersect(y2000, y2025),
#'  y2000, y2025
#' ))
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
#' of a phinterval respectively. Empty (i.e. `<hole>`) time spans are coerced
#' to `NA` datetimes.
#'
#' `phint_starts()` and `phint_ends()` return a list of starts and ends of a
#' phinterval respectively. Empty time spans are returned as length 0
#' `<POSIXct>` elements.
#'
#' @inheritParams params
#'
#' @return
#'
#' For `phint_start()` and `phint_end()`, a `<POSIXct>` vector the same length
#' as `phint`.
#'
#' For `phint_start()` and `phint_ends()`, a list of `<POSIXct>` vectors the
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
#' # Empty <hole> times spans have no start or end date. Time spans containing
#' # gaps have multiple starts and ends.
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
#' negative <- interval(as.Date("1980-01-01"), as.Date("1980-01-01") - 5)
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
#' @details
#'
#' `phint_length()` calculates the total length of time spans within a
#' phinterval. Instantaneous and empty time spans are 0 seconds long.
#'
#' `phint_lengths()` calculates the length in seconds of each time span
#' in a phinterval.
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

# TODO: Document
#' @export
phint_unnest <- function(phint, hole_to = c("drop", "na"), keep_size = FALSE) {
  UseMethod("phint_unnest")
}

#' @export
phint_unnest.Interval <- function(phint, hole_to = c("drop", "na"), keep_size = FALSE) {
  hole_to <- arg_match0(hole_to, values = c("drop", "na"))
  check_bool(keep_size)
  intvl_unnest_cpp(
    starts = lubridate::int_start(phint),
    spans = lubridate::int_length(phint),
    tzone = get_tzone(phint),
    hole_to = hole_to,
    keep_size = keep_size
  )
}

#' @export
phint_unnest.phinterval <- function(phint, hole_to = c("drop", "na"), keep_size = FALSE) {
  hole_to <- arg_match0(hole_to, values = c("drop", "na"))
  check_bool(keep_size)
  phint_unnest_cpp(
    size = field(phint, "size"),
    starts = field(phint, "starts"),
    ends = field(phint, "ends"),
    tzone = get_tzone(phint),
    hole_to = hole_to,
    keep_size = keep_size
  )
}

#' Convert a phinterval to a duration
#'
#' @description
#'
#' `as_duration()` changes [lubridate::interval()] and `<phinterval>` vectors
#' into [lubridate::duration()] vectors. The resulting duration measures the
#' length of time in seconds within each element of the interval or phinterval.
#'
#' `as_duration()` is a wrapper around [lubridate::as.duration()].
#'
#' @param x An object to convert.
#' @param ... Parameters passed to other methods. Currently unused.
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
