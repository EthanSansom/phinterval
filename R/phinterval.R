# todos ------------------------------------------------------------------------

# TODO: pillar formatting and `phinterval.pillar_max_width`

# TODO: Object naming scheme:
# - phint_operate(phint, phint, fn) -> parallel, returns a phinterval
# - phint_relate(phint, phint, fn)  -> parallel, returns a logical
# - phint_modify(phint, fn)         -> single, returns a phinterval
# - intvl_operate/relate/modify()   -> same, input is an interval, fast track

# TODO: Remaining functionality
# - operations: union, setdiff
# - modifications: invert, complement, sift
# - relations: within, overlaps

# TODO: datetime_groups(start, end, by)
# - Wrapper around `datetime_squash(start, end, by) |> phint_unnest()`
# - Faster because we skip the list assignment while creating a <phinterval>
#
# NOTE: Maybe put this off until an update.

# TODO: Dealing with infinite dates in C++
# - Check that IntvlVector and DtimeVector deal with infinite starts, ends, and spans

# TODO: Stricter timezone treatment
# I think either use `OlsonNames()` or a `valid_timezones <- c(...)` constant
# and then `test_tzone(x) { x %in% valid_timezones }`. Have a `valid_tzones()`
# export which returns a vector (or dataframe) of time-zones. The data.frame
# version could have `~tz`, ~`tz_parent`, where `tz_parent` is the equivalent
# timezone (e.g. "EST" is a `tz_parent` of "America New York/Toronto").
#
# Document this in the package "Getting Started". The fix is just to use
# `with_tz()` before converting to phinterval (if needed).

# TODO: Poke into C++ functionality to make sure it's doing what you want
# - Check the `na_view()` from `SetView()` and `SpanView()`

# TODO: Add a section on the {lubridate} quirks of the package
# - Weird stuff with intersect, setdiff, etc. and instants or abutting times
# - Allowing any timezone to be used

# constructors -----------------------------------------------------------------

setOldClass(c("phinterval", "list", "vctrs_rcrd"))

# TODO: Documentation
#' @export
phinterval <- function(start = POSIXct(), end = POSIXct(), tzone = NULL, by = NULL) {
  # TODO: Input validation
  # TODO: Decide on timezone error or coercion
  # - Ugh, I feel like it's best to allow whatever timezones, as lubridate does...

  tzone <- tzone %||% tz_union(start, end)
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  if (is.null(by)) {
    out <- as_phint_datetime_cpp(starts = start, ends = end)
  } else {
    groups <- vec_group_loc(by)
    out <- datetime_squash_by_cpp(
      starts = start,
      ends = end,
      group_locs = groups[["loc"]],
      na_rm = TRUE
    )
  }

  new_phinterval_bare(out, tzone = tzone)
}

new_phinterval <- function(
    size = integer(),
    starts = list(),
    ends = list(),
    tzone = "UTC"
) {
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
  new_rcrd(
    fields = fields,
    tzone = tzone,
    class = "phinterval"
  )
}

new_empty_phinterval <- function(tzone = "UTC") {
  out <- `empty_phinterval!`
  attr(out, "tzone") <- tzone
  out
}

new_na_phinterval <- function(tzone = "UTC") {
  out <- `na_phinterval!`
  attr(out, "tzone") <- tzone
  out
}

new_hole <- function(tzone = "UTC") {
  out <- `hole!`
  attr(out, "tzone") <- tzone
  out
}

`empty_phinterval!` <- new_rcrd(
  fields = list(size = integer(), starts = list(), ends = list()),
  tzone = "UTC",
  class = "phinterval"
)

`na_phinterval!` <- new_rcrd(
  fields = list(size = NA_integer_, starts = list(NULL), ends = list(NULL)),
  tzone = "UTC",
  class = "phinterval"
)

`hole!` <- new_rcrd(
  fields = list(size = 0L, starts = list(numeric()), ends = list(numeric())),
  tzone = "UTC",
  class = "phinterval"
)

# formatting -------------------------------------------------------------------

#' @export
obj_print_data.phinterval <- function(x, max_width = getOption("phinterval.print_max_width"), ...) {
  check_number_whole(max_width, min = 1)
  if (length(x) == 0) {
    return(invisible(x))
  }

  # Truncating prior to formatting as format.phinterval() is slow
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
format.phinterval <- function(x, max_width = getOption("phinterval.print_max_width"), ...) {
  # TODO: Input validation

  # <phinterval> vectors have 3 potential formats
  # - Set:   {YYYY-MM-DD HH:MM:SS--YYYY-MM-DD HH:MM:SS, ...}
  # - Span:  {YYYY-MM-DD HH:MM:SS-[size]-YYYY-MM-DD HH:MM:SS}
  # - Terse: <phint[size]>

  size <- field(x, "size")

  datetime_width <- 20L
  span_width <- (datetime_width * 2L) + 7L
  set_width <- datetime_width * 2L * max(size, na.rm = TRUE)

  if (max_width >= set_width) {
    format_type <- "set"
  } else if (max_width >= span_width) {
    format_type <- "span"
  } else {
    format_type <- "terse"
  }

  switch(
    format_type,
    terse = format_terse(size),
    set = format_set(size, phint_starts(x), phint_ends(x)),
    span = format_span(size, phint_start(x), phint_end(x))
  )
}

format_terse <- function(size) {
  out <- paste0("<phint[", size, "]>")
  after_format(out, size)
}

format_span <- function(size, start, end) {
  start <- format(start, usetz = FALSE)
  end <- format(end, usetz = FALSE)

  out <- paste0("{", start, "-[", size, "]-", end, "}")
  out <- gsub("[1]", "", out, fixed = TRUE)
  after_format(out, size)
}

format_set <- function(size, starts, ends) {
  out <- paste0("{", map2_chr(
    map(starts, function(starts) format(starts, usetz = FALSE)),
    map(ends, function(ends) format(ends, usetz = FALSE)),
    # <hole> elements are initially formatted as `character(0L)`
    function(starts, ends) paste(starts, ends, sep = "--", collapse = ", ") %0|% ""
  ), "}")
  after_format(out, size)
}

after_format <- function(out, size) {
  out[size == 0] <- "<hole>"
  out[is.na(size)] <- NA_character_
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

# vctrs ------------------------------------------------------------------------

# TODO: Think about how invalid timezones interact here!

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
  new_phinterval_bare(
    fields = as_phint_intvl_cpp(x),
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
  check_is_phintish(phint)
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
    ends = field(phint, "ends"),
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
  check_is_phintish(phint)
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
    starts = field(phint, "starts"),
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
  phint_starts_cpp(
    size = field(phint, "size"),
    starts = field(phint, "starts"),
    ends = field(phint, "ends"),
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
  phint_ends_cpp(
    size = field(phint, "size"),
    starts = field(phint, "starts"),
    ends = field(phint, "ends"),
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
  check_is_phintish(phint)
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
  check_is_phintish(phint)
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
# TODO: Input validation
#' @export
phint_unnest <- function(phint, hole_to = c("drop", "na"), keep_size = FALSE) {
  UseMethod("phint_unnest")
}

#' @export
phint_unnest.Interval <- function(phint, hole_to = c("drop", "na"), keep_size = FALSE) {
  hole_to <- arg_match(hole_to)
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
  hole_to <- arg_match0(hole_to, values = c("drop", "na")) # TODO: arg_match0() everywhere?
  phint_unnest_cpp(
    size = field(phint, "size"),
    starts = field(phint, "starts"),
    ends = field(phint, "ends"),
    tzone = get_tzone(phint),
    hole_to = hole_to,
    keep_size = keep_size
  )
}

# miscellaneous ----------------------------------------------------------------

# TODO: All of these need re-factoring for the new <phinterval> data structure.

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
#' phint_invert(phint_squash(c(monday, friday, sunday)))
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
#' phint_to_spans(hole, hole_to = "null")
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
