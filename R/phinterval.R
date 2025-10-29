setOldClass("phinterval")

# phinterval class -------------------------------------------------------------

#' @export
phinterval <- function(intervals = NULL, tzone = NULL) {

  if (rlang::is_empty(intervals)) {
    tzone <- tzone %||% "UTC"
    stop_wrong_class(tzone, "character", n = 1)
    return(new_phinterval(tzone = tzone))
  }

  if (lubridate::is.interval(intervals)) {
    intervals <- list(intervals)
  } else {
    stop_not_list_of(intervals, "Interval")
  }

  # TODO: Make a `get_tzone`, `tzone`, `tz`, generic that "just works"
  #
  # Defaulting to the timezone of the first Interval supplied
  tzone <- tzone %||% interval_tzone(intervals[[1]])
  stop_wrong_class(tzone, "character", n = 1)

  # TODO: From this point on, you should `vctrs::list_unchop` the supplied
  # interval vectors and work in a vectorized way. As in `phint_to_spans`.
  # - this way, operations like the `int_standardize` below can take place
  #   on a single integer
  #   - I don't think you even need to call `int_standardize`. Just reverse
  #     the start and ends yourself if start > end.

  ints <- map(intervals, lubridate::int_standardize)

  # If intervals[[i]] is empty, setting [[i]] to NA. Removing empty intervals
  # would make the output length harder to predict
  non_na_at <- !map_lgl(ints, \(int) rlang::is_empty(int) || any(is.na(int)))
  non_na_ints <- ints[non_na_at]

  reference_time <- na_posixct(length(ints), tzone = tzone)
  range_starts <- range_ends <- as.list(rep(NA_real_, length(ints)))

  # TODO Ethan: Replace all of this with some nice function like, `recenter_ranges`
  #             or something.
  min_start_time <- map_dbl(non_na_ints, \(int) min(lubridate::int_start(int)))
  reference_time[non_na_at] <- lubridate::as_datetime(min_start_time)
  range_starts[non_na_at] <- map2(
    non_na_ints,
    min_start_time,
    \(int, min_start) {
      as.double(lubridate::int_start(int)) - min_start
    }
  )
  range_ends[non_na_at] <- map2(
    non_na_ints,
    min_start_time,
    \(int, min_start) {
      as.double(lubridate::int_end(int)) - min_start
    }
  )

  ranges <- flatten_overlapping_ranges(range_starts, range_ends)

  new_phinterval(
    reference_time = reference_time,
    range_starts = ranges$starts,
    range_ends = ranges$ends,
    tzone = tzone
  )
}

new_phinterval <- function(
    reference_time = empty_posixct(tzone = tzone),
    range_starts = list(),
    range_ends = list(),
    tzone = "UTC"
  ) {

  bullets <- character()
  if (!lubridate::is.POSIXct(reference_time)) {
    bullets <- c(
      bullets,
      paste0(
        "{.var reference_time} must be a {.cls {POSIXct}}, ",
        "not a {.cls {class(reference_time)}."
      )
    )
  }
  if (!(is.list(range_starts) && all(map_lgl(range_starts, is.numeric)))) {
    bullets <- c(
      bullets,
      "{.var range_starts} must be a list of numeric vectors."
    )
  }
  if (!(is.list(range_ends) && all(map_lgl(range_ends, is.numeric)))) {
    bullets <- c(
      bullets,
      "{.var range_ends} must be a list of numeric vectors."
    )
  }
  if (!rlang::is_scalar_character(tzone)) {
    bullets <- c(
      bullets,
      "{.var tzone} must be a scalar character vector."
    )
  }
  if (lubridate::tz(reference_time) != tzone) {
    bullets <- c(
      bullets,
      "{.var reference_time} must be have timezone {.var tzone}."
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
na_phinterval <- function(n = 1L, tzone = "UTC") {
  new_phinterval(
    reference_time = rep(NA_POSIXct_, n),
    range_starts = as.list(rep(NA_real_, n)),
    range_ends = as.list(rep(NA_real_, n)),
    tzone = tzone
  )
}

hole_phinterval <- function(n = 1L, tzone = "UTC") {
  if (n == 0) {
    return(
      new_phinterval(
        reference_time = empty_posixct(tzone = tzone),
        range_starts = list(),
        range_ends = list(),
        tzone = tzone
      )
    )
  }
  range <- lapply(seq(n), \(x) numeric())
  new_phinterval(
    reference_time = origin_posixct(n = n, tzone = tzone),
    range_starts = range,
    range_ends = range,
    tzone = tzone
  )
}

#' @export
empty_posixct <- function(tzone = "UTC") {
  as.POSIXct(logical(), tz = tzone)
}

#' @export
na_posixct <- function(n = 1L, tzone = "UTC") {
  as.POSIXct(rep(NA, n), tz = tzone)
}

#' @export
na_interval <- function(n = 1L, tzone = "UTC") {
  na_times <- na_posixct(n = n, tzone = tzone)
  lubridate::interval(start = na_times, end = na_times)
}

#' @export
origin_posixct <- function(n = 1L, tzone = "UTC") {
  as.POSIXct(rep(0L, n), origin = lubridate::origin, tz = tzone)
}

#' @export
format.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")
  tzone <- trimws(format(reference_time, format = " ", usetz = TRUE))

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

#' @export
vec_ptype_abbr.phinterval <- function(x, ...) {
  "phintrvl"
}

# Thanks: https://github.com/r-lib/vctrs/blob/main/R/type-date-time.R
# for the nice datetime ptype.
#
#' @export
vec_ptype_full.phinterval <- function(x, ...) {
  tzone <- tzone(field(x, "reference_time"))
  if (tz_is_local(tzone)) tzone <- "local"
  paste0("phinterval<", tzone, ">")
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
is_phinterval <- function(x) {
  inherits(x, "phinterval")
}

#' @export
as_phinterval <- function(x, ...) {
  UseMethod("as_phinterval")
}

#' @export
as_phinterval.default <- function(x, ...) {
  vec_cast(x, new_phinterval())
}

#' @export
as_phinterval.Interval <- function(x, tzone = NULL) {

  tzone <- tzone %||% interval_tzone(x)
  stop_wrong_class(tzone, "character", n = 1L)

  int <- lubridate::int_standardize(x)
  na_at <- is.na(int)

  reference_time <- lubridate::with_tz(lubridate::int_start(int), tzone = tzone)
  range_starts <- as.list(rep(0, length(int)))
  range_ends <- as.list(as.double(lubridate::int_length(int)))

  reference_time[na_at] <- lubridate::NA_POSIXct_
  range_starts[na_at] <- NA_real_
  range_ends[na_at] <- NA_real_

  new_phinterval(
    reference_time = reference_time,
    range_starts = range_starts,
    range_ends = range_ends,
    tzone = tzone
  )
}

# TODO: Consolidate the `range_is_flat` and `range_contains_overlaps` functions.
#       Think about the best language to describe overlapping vs. flat vs. intersecting
flatten_overlapping_ranges <- function(range_starts, range_ends) {

  range_not_flat <- !map2_lgl(range_starts, range_ends, range_is_flat)
  range_not_flat[is.na(range_not_flat)] <- FALSE

  if (any(range_not_flat)) {
    flattened <- map2(
      range_starts[range_not_flat],
      range_ends[range_not_flat],
      range_flatten
    )
    range_starts[range_not_flat] <- map(flattened, `[[`, "starts")
    range_ends[range_not_flat] <- map(flattened, `[[`, "ends")
  }

  list(starts = range_starts, ends = range_ends)
}

# `phinterval`s are equal when they represent the same collection of time-spans,
# ignoring timezone. This is unlike `Interval` equality, which is determined by
# duration of the time-span. https://github.com/tidyverse/lubridate/issues/1135

#' @export
vec_proxy_equal.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")

  starts_uid <- rep(NA_character_, length(reference_time))
  ends_uid <- starts_uid

  # TODO: This feels illegal, but should (?) uniquely ID a set of time spans
  non_na_at <- !is.na(reference_time)
  seconds <- as.double(reference_time)[non_na_at]
  starts <- range_starts[non_na_at]
  ends <- range_ends[non_na_at]

  starts_uid[non_na_at] <- map2(seconds, starts, \(t, s) t + sort(s)) |> map_chr(range_to_uid)
  ends_uid[non_na_at] <- map2(seconds, ends, \(t, e) t + sort(e)) |> map_chr(range_to_uid)

  data.frame(
    range_starts = starts_uid,
    range_ends = ends_uid
  )
}

range_to_uid <- function(x) paste0(sort(x), collapse = ",")

#' @export
vec_proxy_compare.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")

  min_start <- map2_dbl(reference_time, range_starts, \(t, s) t + min(s))
  max_end <- map2_dbl(reference_time, range_ends, \(t, e) t + max(e))
  duration_seconds <- map2_dbl(range_starts, range_ends, \(s, e) sum(e - s))

  # `phinterval`s are ordered by start, end, and duration.
  data.frame(
    starts_first = min_start,
    ends_first = -max_end,
    shortest = -duration_seconds
  )
}

# division ---------------------------------------------------------------------

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

# phinterval interface ---------------------------------------------------------

#' @export
is_holey <- function(phint, na_as = NA) {
  if (lubridate::is.interval(phint)) {
    out <- rep(FALSE, length(phint))
  } else {
    phint <- check_is_phinty(phint)
    range_starts <- field(phint, "range_starts")
    out <- map_lgl(range_starts, \(s) length(s) > 1)
  }

  out[is.na(phint)] <- na_as
  out
}

# This is the case where the phinterval element is an empty set of intervals.
#
#' @export
is_hole <- function(phint, na_as = NA) {
  if (lubridate::is.interval(phint)) {
    out <- rep(FALSE, length(phint))
  } else {
    phint <- check_is_phinty(phint)
    range_starts <- field(phint, "range_starts")
    out <- map_int(range_starts, length) <= 0
  }
  out[is.na(phint)] <- na_as
  out
}

#' @export
n_spans <- function(phint, nas_as = NA) {

  if (lubridate::is.interval(phint)) {
    out <- rep(1L, length(phint))
  } else {
    phint <- check_is_phinty(phint)
    range_starts <- field(phint, "range_starts")
    out <- map_int(range_starts, length)
  }

  out[is.na(phint)] <- NA
  out
}

#' @export
n_holes <- function(phint, na_as = NA) {
  n_spans(phint, na_as) - 1L
}

#' @export
phint_start <- function(phint) {
  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  reference_time + map_dbl(range_starts, min)
}

#' @export
phint_starts <- function(phint) {

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")

  out <- as.list(na_posixct(length(phint), tzone = tz(phint)))
  non_na_at <- !is.na(phint)

  out[non_na_at] <- map2(
    reference_time[non_na_at],
    range_starts[non_na_at],
    \(t, s) t + sort(s)
  )
  out
}

#' @export
phint_end <- function(phint) {
  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_ends <- field(phint, "range_ends")
  reference_time + map_dbl(range_ends, max)
}

#' @export
phint_ends <- function(phint) {

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_ends <- field(phint, "range_ends")

  out <- as.list(na_posixct(length(phint), tzone = tz(phint)))
  non_na_at <- !is.na(phint)

  out[non_na_at] <- map2(
    reference_time[non_na_at],
    range_ends[non_na_at],
    \(t, e) t + sort(e)
  )
  out
}

#' @export
phint_length <- function(phint) {
  map_dbl(phint_lengths(phint), sum)
}

#' @export
phint_lengths <- function(phint) {

  phint <- check_is_phinty(phint)
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")

  out <- as.list(rep(NA_real_, length(phint)))
  non_na_at <- !is.na(phint)

  out[non_na_at] <- map2(
    range_starts[non_na_at],
    range_ends[non_na_at],
    \(s, e) sort(e - s)
  )
  out
}

#' @export
phint_invert <- function(phint) {

  if (lubridate::is.interval(phint)) {
    tzone <- interval_tzone(phint)
    return(na_phinterval(n = length(phint), tzone = tzone))
  }

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- tz(phint)

  is_holey <- !(is.na(phint) | map_lgl(range_starts, \(s) length(s) == 1))

  holey_ranges <- order_ranges(range_starts[is_holey], range_ends[is_holey])
  hole_starts <- map(holey_ranges$ends, \(e) e[-length(e)])
  hole_ends <- map(holey_ranges$starts, \(s) s[-1L])

  reference_time[!is_holey] <- na_posixct(1L, tzone = tzone)

  new_starts <- as.list(rep(NA_real_, length(phint)))
  new_ends <- new_starts
  new_starts[is_holey] <- hole_starts
  new_ends[is_holey] <- hole_ends

  new_phinterval(
    reference_time = reference_time,
    range_starts = new_starts,
    range_ends = new_ends,
    tzone = tzone
  )
}

order_ranges <- function(range_starts, range_ends) {
  starts_order <- map(range_starts, order)
  starts <- map2(range_starts, starts_order, \(s, o) s[o])
  ends <- map2(range_ends, starts_order, \(e, o) e[o])
  list(starts = starts, ends = ends)
}

# TODO: Where possible the approach that got us from `phint_to_spans_v1` to
# the current iteration should be taken. In particular, work with vectors instead
# of lists of vectors where possible (i.e. avoid `map`).
#
# In the current `phint_to_spans`, we avoid creating a ton of intervals in
# the V1 `map2(int_starts, int_ends, interval, tzone = tzone)` call by creating
# a single interval and then splitting it.
#
# The function below transforms a `phinterval` into a list of parallel of POSIX
# starts and ends, with their `index` in the phinterval. Empty (hole) elements
# are NOT included. I.e. You'd assign this back to the non-hole elements.
flatten_phinterval <- function(phint) {

  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts") |> map2(reference_time, `+`)
  range_ends <- field(phint, "range_ends") |> map2(reference_time, `+`)

  n_spans <- vctrs::list_sizes(range_starts)
  list(
    index = rep(seq_along(n_spans), times = n_spans),
    range_starts = vctrs::list_unchop(range_starts),
    range_ends = vctrs::list_unchop(range_ends)
  )

}

# - Use `vctrs::list_unchop` instead of `list_c`
# - Don't bother with `standardize_phinterval`
#' @export
phint_to_spans <- function(phint, hole_to = c("null", "na")) {

  phint <- check_is_phinty(phint)
  hole_to <- rlang::arg_match(hole_to)

  reference_time <- field(phint, "reference_time")
  int_starts <- field(phint, "range_starts") |> map2(reference_time, `+`)
  int_ends <- field(phint, "range_ends") |> map2(reference_time, `+`)

  n_spans <- lengths(int_starts)
  spans <- interval(
    vctrs::list_unchop(int_starts),
    vctrs::list_unchop(int_ends),
    tzone = tz(phint)
  )

  # There will be no elements of `split(spans, ...)` generated for empty
  # phintervals (holes), so we can just ignore them. The holes will be an empty
  # (NULL) list element in `out` which makes sense.
  out <- vector("list", length(phint))
  out[n_spans > 0] <- split(spans, rep(seq_along(n_spans), times = n_spans))
  if (hole_to  == "na") {
    out[n_spans <= 0] <- list(na_interval(1L))
  }
  out
}

#' @export
phint_to_holes <- function(phint) {
  phint |>
    check_is_phinty() |>
    phint_invert() |>
    phint_to_spans()
}

# set operations ---------------------------------------------------------------

# TODO Ethan: You might want an option to return NA on empty, so
#             that you can use this in a `summarize` statement
#             and reliably return a length-1 output. Maybe switch
#             na and empty in `empty`

# TODO: Re-test this and update the version that you're using.

#' @export
phint_squash <- function(phint, na.rm = TRUE, empty = c("na", "empty", "hole")) {

  stop_wrong_class(na.rm, "logical", n = 1L)
  empty <- rlang::arg_match(empty)
  phint <- check_is_phinty(phint)
  tzone <- tz(phint)

  if (rlang::is_empty(phint)) {
    return(switch(
      empty,
      "empty" = phinterval(tzone = tzone),
      "na" = na_phinterval(tzone = tzone),
      "hole" = hole_phinterval(tzone = tzone)
    ))
  }
  if (lubridate::is.interval(phint)) {
    return(int_squash(phint, na.rm = na.rm))
  }

  reference_seconds <- as.double(field(phint, "reference_time"))
  range_starts <- field(phint, "range_starts") |> map2(reference_seconds, `+`)
  range_ends <- field(phint, "range_ends") |> map2(reference_seconds, `+`)

  na_at <- is.na(reference_seconds)
  if (all(na_at) || (any(na_at) && !na.rm)) {
    return(na_phinterval(tzone = tzone))
  }
  if (na.rm) {
    range_starts <- range_starts[!na_at]
    range_ends <- range_ends[!na_at]
  }

  flat_ranges <- range_flatten(list_c(range_starts), list_c(range_ends))
  new_phinterval(
    reference_time = origin_posixct(1L, tzone = tzone),
    range_starts = list(flat_ranges$starts),
    range_ends = list(flat_ranges$ends),
    tzone = tzone
  )

}

#' @export
int_squash <- function(int, na.rm = TRUE) {

  int <- lubridate::int_standardize(int)
  int_starts <- lubridate::int_start(int)
  int_ends <- lubridate::int_end(int)
  tzone <- lubridate::tz(int_starts)

  na_at <- is.na(int)
  if (all(na_at) || (any(na_at) && !na.rm)) {
    return(na_phinterval(tzone = tzone))
  }
  if (na.rm) {
    int_starts <- int_starts[!na_at]
    int_ends <- int_ends[!na_at]
  }

  flat_ranges <- range_flatten(as.double(int_starts), as.double(int_ends))
  new_phinterval(
    reference_time = origin_posixct(1L, tzone = tzone),
    range_starts = list(flat_ranges$starts),
    range_ends = list(flat_ranges$ends),
    tzone = tzone
  )
}

#' @export
phint_overlaps <- function(phint1, phint2, inclusive = FALSE) {

  objs <- recycle2_common(phint1, phint2)
  phint1 <- check_is_phinty(objs$x)
  phint2 <- check_is_phinty(objs$y)

  range1 <- rangify_phinterval(phint1)
  range2 <- rangify_phinterval(phint2)

  non_na_at <- !(is.na(phint1) | is.na(phint2))

  range1_starts <- range1$starts[non_na_at]
  range1_ends <- range1$ends[non_na_at]
  range2_starts <- range2$starts[non_na_at]
  range2_ends <- range2$ends[non_na_at]

  out <- rep(NA, length(phint1))
  out[non_na_at] <- pmap_lgl(
    list(
      x_starts = range1_starts,
      x_ends = range1_ends,
      y_starts = range2_starts,
      y_ends = range2_ends
    ),
    range_intersects,
    inclusive = inclusive
  )
  out
}

#' @export
phint_union <- function(phint1, phint2) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_union
  )
}

#' @export
phint_intersect <- function(phint1, phint2, inclusive = FALSE) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_intersect,
    inclusive = inclusive
  )
}

#' @export
phint_diff <- function(phint1, phint2) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_setdifference
  )
}

# helpers ----------------------------------------------------------------------

combine_phintervals <- function(.phint1, .phint2, .f, ...) {

  objs <- recycle2_common(.phint1, .phint2)
  phint1 <- check_is_phinty(objs$x)
  phint2 <- check_is_phinty(objs$y)

  range1 <- rangify_phinterval(phint1)
  range2 <- rangify_phinterval(phint2)

  non_na_at <- !(is.na(phint1) | is.na(phint2))

  range1_starts <- range1$starts[non_na_at]
  range1_ends <- range1$ends[non_na_at]
  range2_starts <- range2$starts[non_na_at]
  range2_ends <- range2$ends[non_na_at]

  out_range <- pmap(
    list(
      x_starts = range1_starts,
      x_ends = range1_ends,
      y_starts = range2_starts,
      y_ends = range2_ends
    ),
    .f = .f,
    ...
  )

  out_length <- length(phint1)

  tzone <- tz_union(phint1, phint2)
  reference_time <- origin_posixct(out_length, tzone = tzone)
  range_starts <- as.list(rep(NA_real_, out_length))
  range_ends <- range_starts

  range_starts[non_na_at] <- map(out_range, `[[`, "starts")
  range_ends[non_na_at] <- map(out_range, `[[`, "ends")

  new_phinterval(
    reference_time = reference_time,
    range_starts = range_starts,
    range_ends = range_ends,
    tzone = tzone
  )
}

rangify_phinterval <- function(phint) {

  reference_seconds <- as.double(field(phint, "reference_time"))
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")

  list(
    starts = map2(reference_seconds, range_starts, `+`),
    ends = map2(reference_seconds, range_ends, `+`)
  )
}

standardize_phinterval <- function(phint) {

  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- tz(phint)

  # Temporarily assign holes a range of [0, 0] so they can pass through
  # the `order` and `min` operations without problems.
  holes <- is_hole(phint, na_as = FALSE)
  range_starts[holes] <- list(0)
  range_ends[holes] <- list(0)

  starts_order <- map(range_starts, order)
  starts_min <- map_dbl(range_starts, min)

  new_time <- reference_time + starts_min
  new_starts <- pmap(
    list(range_starts, starts_order, starts_min),
    \(range_starts, starts_order, starts_min) {
      range_starts[starts_order] - starts_min
    }
  )
  new_ends <- pmap(
    list(range_ends, starts_order, starts_min),
    \(range_ends, starts_order, starts_min) {
      range_ends[starts_order] - starts_min
    }
  )

  # Reset the holes to be empty
  new_starts[holes] <- list(numeric())
  new_ends[holes] <- list(numeric())

  new_phinterval(
    reference_time = new_time,
    range_starts = new_starts,
    range_ends = new_ends,
    tzone = tzone
  )
}
