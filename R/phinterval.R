setOldClass("phinterval")

#' @export
phinterval <- function(intervals = NULL, tzone = NULL) {

  # TODO: Allow the `intervals` to be phintervals OR intervals
  # TODO: Add input checking for `intervals` and `tzone`
  if (rlang::is_empty(intervals)) {
    tzone <- tzone %||% "UTC"
    return(new_phinterval(tzone = tzone))
  }
  if (lubridate::is.interval(intervals)) {
    tzone <- tzone %||% get_tzone(intervals)
    interval_set <- phint_squash(intervals, na.rm = FALSE)
    return(new_phinterval(interval_sets = interval_set, tzone = tzone))
  }

  stop_not_list_of(intervals, "Interval")
  tzone <- tzone %||% get_tzone(intervals[[1]])
  new_phinterval(
    interval_sets = map(intervals, phint_squash, na.rm = FALSE),
    tzone = tzone
  )
}

new_phinterval <- function(interval_sets = list(), tzone = "UTC") {
  vctrs::new_vctr(
    if (is.matrix(interval_sets)) list(interval_sets) else interval_sets,
    tzone = tzone,
    class = "phinterval"
  )
}

#' @export
format.phinterval <- function(x, max_width = 120, ...) {

  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(x))
  n_intervals <- map(vec_data(x), nrow)
  starts <- map(vec_data(x), \(x) format(x[ , 1] + origin, usetz = FALSE))
  ends <- map(vec_data(x), \(x) format(x[ , 2] + origin, usetz = FALSE))

  out <- paste0("{",
    map2_chr(
      starts,
      ends,
      \(x, y) paste(x, y, sep = "--", collapse = ", ")
    ),
  "}")

  too_wide <- nchar(out) > max_width
  out[too_wide] <- paste0("<phint[", n_intervals[too_wide], "]>")
  out[n_intervals == 0] <- "<hole>"
  out[is.na(starts)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.phinterval <- function(x, ...) {
  "phintrvl"
}

#' @export
vec_ptype_full.phinterval <- function(x, ...) {
  tzone <- get_tzone(x)
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

  tzone <- tzone %||% get_tzone(x)
  stop_wrong_class(tzone, "character", n = 1L)

  starts <- lubridate::int_start(x)
  spans <- lubridate::int_length(x)
  new_phinterval(
    interval_sets = cpp_lubridate_interval_to_interval_sets(starts, spans),
    tzone = tzone
  )
}

#' @export
n_spans <- function(phint) {
  UseMethod("n_spans")
}

#' @export
n_spans.Interval <- function(phint) {
  out <- rep(1L, length(phint))
  out[is.na(phint)] <- NA_integer_
  out
}

#' @export
n_spans.phinterval <- function(phint) {
  out <- map_int(vec_data(phint), nrow)
  out[is.na(phint)] <- NA_integer_
  out
}

#' @export
is_hole <- function(phint) {
  n_spans(phint) == 0L
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
    tzone <- get_tzone(phint)
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

# constructors -----------------------------------------------------------------

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


# helpers ----------------------------------------------------------------------

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
