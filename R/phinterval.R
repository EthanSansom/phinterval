# class ------------------------------------------------------------------------

methods::setOldClass(c("phinterval", "list", "vctrs_vctr"))

#' Create a new phinterval
#'
#' @description
#'
#' `phinterval()` creates a new `<phinterval>` vector from a list of
#' [lubridate::interval()] vectors. A phinterval (think "potentially holey interval")
#' is a span of time which may contain gaps.
#'
#' @details
#'
#' The `<phinterval>` class is designed as a generalization of the
#' [lubridate::Interval-class]. While an `<Interval>` element represents a
#' single contiguous span between two fixed times, a `<phinterval>` element can
#' represent a time span that may be empty, contiguous, or disjoint (i.e. containing
#' gaps). Each element of a `<phinterval>` is stored as a (possibly empty) set of
#' non-overlapping and non-abutting time spans.
#'
#' @param intervals `[list of Interval]`
#'
#' A list of [lubridate::interval()] vectors. Each interval vector is merged
#' into a set of non-overlapping and non-adjacent time spans. Empty (length 0)
#' interval vectors are returned as an empty set of time spans (i.e. a `<hole>`).
#'
#' @param tzone `[character(1)]`
#'
#' A recognized timezone to display the `<phinterval>` in. If `tzone` is `NULL`
#' (the default), then the timezone of the first element of `intervals` is used.
#'
#' @return
#'
#' A `<phinterval>` vector.
#'
#' @examples
#' jan <- interval(as.Date("2000-01-01"), as.Date("2000-02-01"), tz = "UTC")
#' feb <- interval(as.Date("2000-02-01"), as.Date("2000-03-01"), tz = "UTC")
#' nov <- interval(as.Date("2000-11-01"), as.Date("2000-12-01"), tz = "UTC")
#'
#' phinterval(list(jan, c(feb, nov)))
#' phinterval(list(jan), tzone = "EST")
#' phinterval()
#'
#' # Empty (length 0) intervals can be used to create <hole> elements
#' phinterval(list(interval()))
#'
#' # Abutting or overlapping intervals are merged into one span
#' phinterval(list(c(jan, feb)))
#'
#' @export
phinterval <- function(intervals = list(), tzone = NULL) {
  if (!is_bare_list(intervals)) {
    check_is_phintish(intervals)
    intervals <- list(as_phinterval(intervals))
  } else {
    check_is_list_of_phintish(intervals)
    intervals <- map(intervals, as_phinterval)
  }
  check_valid_tzone(tzone, allow_null = TRUE)

  if (is_empty(intervals)) {
    return(new_phinterval(tzone = tzone %||% "UTC"))
  }
  new_phinterval(
    interval_sets = map(intervals, phint_to_interval_set, empty_to = "hole"),
    tzone = tzone %||% get_tzone(intervals[[1]])
  )
}

new_phinterval <- function(interval_sets = list(), tzone = "UTC") {
  new_vctr(
    if (!is.list(interval_sets)) list(interval_sets) else interval_sets,
    tzone = tzone,
    class = "phinterval"
  )
}

#' @export
obj_print_data.phinterval <- function(x, max_width = 90, ...) {
  check_number_whole(max_width, min = 1)
  if (length(x) == 0) {
    return(invisible(x))
  }

  # Truncating prior to formatting as format.phinterval is slow
  max_print <- getOption("max.print", 9999L)
  if (length(x) > max_print) {
    x_t <- x[seq_len(max_print)]
    out <- set_names(format(x_t, max_width = max_width), names(x_t))
    print(out, quote = FALSE)
    cat(" [ Omitted", length(x) - max_print, "entries ]\n")
    return(invisible(x))
  }

  out <- set_names(format(x, max_width = max_width), names(x))
  print(out, quote = FALSE)
  invisible(x)
}

#' @export
format.phinterval <- function(x, max_width = 90, ...) {
  check_number_whole(max_width, min = 1)

  out <- paste0("{",
    map2_chr(
      map(phint_starts(x), function(starts) format(starts, usetz = FALSE)),
      map(phint_ends(x), function(ends) format(ends, usetz = FALSE)),
      # <hole> elements are initially formatted as character(0L)
      function(starts, ends) paste(starts, ends, sep = "--", collapse = ", ") %0|% ""
    ),
  "}")

  n_spans <- n_spans(x)
  too_big <- nchar(out) > max_width
  out[too_big] <- paste0("<phint[", n_spans[too_big], "]>")
  out[!n_spans] <- "<hole>"
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.phinterval <- function(x, ...) {
  tzone <- get_tzone(x)
  if (tz_is_local(tzone)) tzone <- "local" #nocov
  paste0("phint<", tzone, ">")
}

#' @export
vec_ptype_full.phinterval <- function(x, ...) {
  tzone <- get_tzone(x)
  if (tz_is_local(tzone)) tzone <- "local" #nocov
  paste0("phinterval<", tzone, ">")
}

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
  vec_cast(x, new_phinterval())
}

#' @rdname as_phinterval
#' @export
as_phinterval.Interval <- function(x, ...) {
  new_phinterval(
    interval_sets = cpp_lubridate_interval_to_interval_sets(
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
#'  y2000, 2025
#' ))
#'
#' @export
n_spans <- function(phint) {
  UseMethod("n_spans")
}

#' @rdname n_spans
#' @export
n_spans.default <- function(phint) {
  check_is_phintish(phint)
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
  out <- rep(NA_integer_, length(phint))
  non_na <- !is.na(phint)
  out[non_na] <- map_int(vec_data(phint)[non_na], nrow)
  out
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
#'  y2000, 2025
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
  check_is_phintish(phint)
}

#' @rdname phinterval-accessors
#' @export
phint_start.Interval <- function(phint) {
  out <- lubridate::int_start(lubridate::int_standardize(phint))
  out[is.na(phint)] <- NA
  out
}

#' @rdname phinterval-accessors
#' @export
phint_start.phinterval <- function(phint) {
  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(phint))
  cpp_interval_sets_start(vec_data(phint)) + origin
}

#' @rdname phinterval-accessors
#' @export
phint_end <- function(phint) {
  UseMethod("phint_end")
}

#' @rdname phinterval-accessors
#' @export
phint_end.default <- function(phint) {
  check_is_phintish(phint)
}

#' @rdname phinterval-accessors
#' @export
phint_end.Interval <- function(phint) {
  out <- lubridate::int_end(lubridate::int_standardize(phint))
  out[is.na(phint)] <- NA
  out
}

#' @rdname phinterval-accessors
#' @export
phint_end.phinterval <- function(phint) {
  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(phint))
  cpp_interval_sets_end(vec_data(phint)) + origin
}

#' @rdname phinterval-accessors
#' @export
phint_starts <- function(phint) {
  UseMethod("phint_starts")
}

#' @rdname phinterval-accessors
#' @export
phint_starts.default <- function(phint) {
  check_is_phintish(phint)
}

#' @rdname phinterval-accessors
#' @export
phint_starts.Interval <- function(phint) {
  as.list(phint_start(phint))
}

#' @rdname phinterval-accessors
#' @export
phint_starts.phinterval <- function(phint) {
  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(phint))
  map(cpp_interval_sets_starts(vec_data(phint)), `+`, origin)
}

#' @rdname phinterval-accessors
#' @export
phint_ends <- function(phint) {
  UseMethod("phint_ends")
}

#' @rdname phinterval-accessors
#' @export
phint_ends.default <- function(phint) {
  check_is_phintish(phint)
}

#' @rdname phinterval-accessors
#' @export
phint_ends.Interval <- function(phint) {
  as.list(phint_end(phint))
}

#' @rdname phinterval-accessors
#' @export
phint_ends.phinterval <- function(phint) {
  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(phint))
  map(cpp_interval_sets_ends(vec_data(phint)), `+`, origin)
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
  check_is_phintish(phint)
}

#' @rdname phint_length
#' @export
phint_length.Interval <- function(phint) {
  lubridate::int_length(phint)
}

#' @rdname phint_length
#' @export
phint_length.phinterval <- function(phint) {
  map_dbl(phint_lengths(phint), sum)
}

#' @rdname phint_length
#' @export
phint_lengths <- function(phint) {
  UseMethod("phint_lengths")
}

#' @rdname phint_length
#' @export
phint_lengths.default <- function(phint) {
  check_is_phintish(phint)
}

#' @rdname phint_length
#' @export
phint_lengths.Interval <- function(phint) {
  as.list(lubridate::int_length(phint))
}

#' @rdname phint_length
#' @export
phint_lengths.phinterval <- function(phint) {
  data <- vec_data(phint)
  out <- map2(
    cpp_interval_sets_ends(data),
    cpp_interval_sets_starts(data),
    `-`
  )
  out[is_hole(phint)] <- list(0)
  out
}

# miscellaneous ----------------------------------------------------------------

#' Get the gaps in a phinterval as time spans
#'
#' @description
#'
#' `phint_invert()` returns the gaps in a phinterval as a `<phinterval>` vector.
#' Contiguous time spans (e.g. [lubridate::interval()] vectors) are inverted to
#' `<hole>` time spans.
#'
#' `phint_invert()` is similar to `phint_complement()`, except that the time occurring
#' outside the extent of `phint` (i.e. before its earliest start or after its
#' latest end) is not included in the result.
#'
#' @inheritParams params
#'
#' @param hole_to `["hole" / "inf" / "na"]`
#'
#' What to turn `<hole>` (i.e. empty) time spans into.
#' - If `hole_to = "hole"` (the default), `<hole>` spans remain as `<hole>` elements.
#' - If `"inf"`, they are returned as a time span from `-Inf` to `Inf`.
#' - If `"na"`, they are returned as a missing (`NA`) span.
#'
#' @return
#'
#' A `<phinterval>` vector the same length as `phint`.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#' sunday <- interval(as.Date("2025-11-16"), as.Date("2025-11-17"))
#'
#' # Contiguous intervals are inverted to holes,
#' # disjoint intervals to spans
#' phint_invert(monday)
#' phint_invert(phint_squash(c(monday, friday, sunday))
#'
#' tues_to_thurs <- interval(as.Date("2025-11-11"), as.Date("2025-11-14"))
#' phint_invert(phint_union(monday, friday)) == tues_to_thurs
#'
#' # The time before `monday` and after `friday` is included
#' # in the complement, but not the inversion
#' mon_and_fri <- phint_union(monday, friday)
#' phint_invert(mon_and_fri)
#' phint_complement(mon_and_fri)
#'
#' # Specify how to invert empty time spans
#' hole <- phint_intersect(monday, friday)
#' phint_invert(hole, hole_to = "hole")
#' phint_invert(hole, hole_to = "inf")
#' phint_invert(hole, hole_to = "na")
#'
#' @export
phint_invert <- function(phint, hole_to = c("hole", "inf", "na")) {
  UseMethod("phint_invert")
}

#' @rdname phint_invert
#' @export
phint_invert.default <- function(phint, hole_to = c("hole", "inf", "na")) {
  check_is_phintish(phint)
}

#' @rdname phint_invert
#' @export
phint_invert.Interval <- function(phint, hole_to = c("hole", "inf", "na")) {
  arg_match(hole_to)
  interval_sets <- rep(list(the$empty_interval_set), length(phint))
  interval_sets[is.na(phint)] <- list(the$na_interval_set)
  new_phinterval(interval_sets, tzone = get_tzone(phint))
}

#' @rdname phint_invert
#' @export
phint_invert.phinterval <- function(phint, hole_to = c("hole", "inf", "na")) {
  hole_to <- arg_match(hole_to)
  interval_sets <- cpp_invert_interval_sets(vec_data(phint))
  switch(
    hole_to,
    inf = interval_sets[is_hole(phint)] <- list(the$inf_interval_set),
    na = interval_sets[is_hole(phint)] <- list(the$na_interval_set)
  )
  new_phinterval(interval_sets = interval_sets, tzone = get_tzone(phint))
}

#' Convert a phinterval into a list of intervals
#'
#' @description
#'
#' `phint_to_spans()` decomposes each element of a phinterval into the set
#' of its contiguous time spans, returned as [lubridate::interval()] vectors.
#'
#' @inheritParams params
#'
#' @param hole_to `["empty" / "na" / "null"]`
#'
#' What to turn `<hole>` (i.e. empty) time spans into.
#' If `hole_to = "empty"` (the default), `<hole>` spans are returned as length 0 intervals.
#' If `"na"`, they are returned as a missing (`NA`) interval.
#' If `"null"`, they are returned as a `NULL` element.
#'
#' @return
#'
#' A list of `<Interval>` vectors the same length as `phint`. If `hole_to = "null"`,
#' the list may contain `NULL` elements.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#' sunday <- interval(as.Date("2025-11-16"), as.Date("2025-11-17"))
#'
#' mon_and_fri <- phint_union(monday, friday)
#' phint_to_spans(mon_and_fri)
#' phint_to_spans(sunday)
#'
#' # Specify how to invert empty time spans
#' hole <- phint_intersect(monday, friday)
#' phint_to_spans(hole, hole_to = "empty")
#' phint_to_spans(hole, hole_to = "na")
#' phint_to_spans(hole, hole_to = "NULL")
#'
#' @export
phint_to_spans <- function(phint, hole_to = c("empty", "na", "null")) {
  hole_to <- arg_match(hole_to)
  out <- map2(phint_starts(phint), phint_ends(phint), interval)
  switch(
    hole_to,
    null = out[is_hole(phint)] <- list(NULL),
    na = out[is_hole(phint)] <- list(interval(NA, NA, tzone = get_tzone(phint)))
  )
  out
}

#' Remove instantaneous time spans from a phinterval
#'
#' @description
#'
#' `phint_sift()` removes instants (i.e. spans of 0 seconds in duration) from
#' elements of a phinterval.
#'
#' @inheritParams params
#' @return A `<phinterval>` vector the same length as `phint`.
#'
#' @examples
#' y2020 <- interval(as.Date("2020-01-01"), as.Date("2021-01-01"))
#' y2021 <- interval(as.Date("2021-01-01"), as.Date("2022-01-01"))
#' y2022 <- interval(as.Date("2022-01-01"), as.Date("2023-01-01"))
#'
#' # The intersection of two adjacent intervals is an instant.
#' # phint_sift() is useful for removing these instants.
#' (new_years_2021 <- phint_intersect(y2020, y2021))
#' phint_sift(new_years_2021)
#'
#' (y2022_and_new_years_2021 <- phint_union(y2022, new_years_2021))
#' phint_sift(y2022_and_new_years_2021)
#'
#' @export
phint_sift <- function(phint) {
  check_is_phintish(phint)
  new_phinterval(
    cpp_interval_sets_remove_instants(vec_data(as_phinterval(phint))),
    tzone = get_tzone(phint)
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
