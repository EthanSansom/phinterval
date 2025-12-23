#' Vectorized set operations
#'
#' @description
#'
#' These functions perform elementwise set operations on `<phinterval>` vectors,
#' treating each element as a set of non-overlapping intervals. They return a new
#' `<phinterval>` vector representing the result of the corresponding set
#' operation. All functions follow vctrs-style recycling rules.
#'
#' - `phint_complement()` returns all time spans *not covered* by `phint`.
#' - `phint_union()` returns the intervals that are within either `phint1` or `phint2`.
#' - `phint_intersect()` returns the intervals that are within both `phint1` and `phint2`.
#' - `phint_setdiff()` returns intervals in `phint1` that are not within `phint2`.
#'
#' @inheritParams params
#' @return A `<phinterval>` vector.
#'
#' @examples
#' monday  <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
#' friday  <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#' jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
#' jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
#'
#' phint_complement(jan_1_to_5)
#'
#' # The complement of a <hole> is an infinite span covering all dates
#' hole <- phinterval(interval())
#' phint_complement(hole)
#'
#' phint_union(c(monday, monday, monday), c(tuesday, friday, NA))
#'
#' # Elements of length 1 are recycled
#' phint_union(monday, c(tuesday, friday, NA))
#'
#' phint_intersect(jan_1_to_5, jan_3_to_9)
#'
#' # The intersection of non-overlapping intervals is a <hole>
#' phint_intersect(monday, friday)
#'
#' # The intersection of adjacent intervals is an instant
#' phint_intersect(monday, tuesday)
#'
#' phint_setdiff(jan_1_to_5, jan_3_to_9)
#' phint_setdiff(jan_3_to_9, jan_1_to_5)
#'
#' # Instantaneous intervals do not affect the set difference
#' noon_monday <- as.POSIXct("2025-11-10 12:00:00")
#' phint_setdiff(monday, interval(noon_monday, noon_monday)) == monday
#'
#' @name phinterval-set-operations
NULL

#' @rdname phinterval-set-operations
#' @export
phint_complement <- function(phint) {
  check_is_phintish(phint)
  new_phinterval(
    cpp_complement_interval_sets(phint_data(phint)),
    tzone = get_tzone(phint)
  )
}

#' @rdname phinterval-set-operations
#' @export
phint_union <- function(phint1, phint2) {
  phints <- validate_phints(phint1, phint2)
  new_phinterval(
    cpp_union_interval_sets(phint_data(phints[[1]]), phint_data(phints[[2]])),
    tzone = tz_union(phint1, phint2)
  )
}

#' @rdname phinterval-set-operations
#' @export
phint_intersect <- function(phint1, phint2) {
  phints <- validate_phints(phint1, phint2)
  new_phinterval(
    cpp_intersect_interval_sets(phint_data(phints[[1]]), phint_data(phints[[2]])),
    tzone = tz_union(phint1, phint2)
  )
}

#' @rdname phinterval-set-operations
#' @export
phint_setdiff <- function(phint1, phint2) {
  phints <- validate_phints(phint1, phint2)
  new_phinterval(
    cpp_setdiff_interval_sets(phint_data(phints[[1]]), phint_data(phints[[2]])),
    tzone = tz_union(phint1, phint2)
  )
}

# squash -----------------------------------------------------------------------

#' Flatten a phinterval
#'
#' @description
#'
#' `phint_squash()` combines all elements of a `<phinterval>` vector into a
#' single set of non-overlapping, non-adjacent time spans, returned as a
#' scalar `<phinterval>` by default.
#'
#' This is useful in conjunction with aggregation functions, e.g.
#' [dplyr::summarize()].
#'
#' @inheritParams params
#'
#' @param na.rm `[TRUE / FALSE]`
#'
#' Whether to ignore `NA` elements when flattening a `<phinterval>`.
#'
#' If `na.rm = FALSE` and `phint` contains any `NA` elements, then a scalar `NA`
#' `<phinterval>` will be returned.
#'
#' @param empty_to
#'
#' What to return if the input `phint` is length 0.
#' - If `empty_to = "na"` (the default), a length 1 `NA` `<phinterval>` is returned.
#' - If `"hole"` a single `<hole>` is returned.
#' - If `"empty"` a length 0 `<phinterval>` vector is returned.
#'
#' @return
#'
#' A length 1 `<phinterval>` vector, with one exception.
#'
#' If the input is an empty `<phinterval>` or `<Interval>` vector *and*
#' `empty_to = "empty"`, then the result is a length 0 `<phinterval>` vector.
#'
#' @examples
#' jan_1_to_5   <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
#' jan_3_to_9   <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
#' jan_11_to_12 <- interval(as.Date("2000-01-11"), as.Date("2000-01-12"))
#'
#' phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA))
#'
#' # Set `na.rm = FALSE` to propagate `NA` values
#' phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA), na.rm = FALSE)
#'
#' # `empty_to` determines the result of empty inputs
#' empty <- interval()
#' phint_squash(empty, empty_to = "na")
#' phint_squash(empty, empty_to = "hole")
#' phint_squash(empty, empty_to = "empty")
#'
#' @export
phint_squash <- function(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty")) {
  UseMethod("phint_squash")
}

#' @export
phint_squash.default <- function(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty")) {
  check_is_phintish(phint)
}

#' @export
phint_squash.phinterval <- function(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty")) {
  check_bool(na.rm)
  empty_to <- arg_match(empty_to)
  new_phinterval(
    interval_sets = phint_to_interval_set(phint, na.rm = na.rm, empty_to = empty_to),
    tzone = get_tzone(phint)
  )
}

#' @export
phint_squash.Interval <- function(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty")) {
  check_bool(na.rm)
  empty_to <- arg_match(empty_to)
  new_phinterval(
    interval_sets = int_to_interval_set(phint, na.rm = na.rm, empty_to = empty_to),
    tzone = get_tzone(phint)
  )
}

phint_to_interval_set <- function(phint, na.rm = TRUE, empty_to = "") {
  if (!na.rm && anyNA(phint)) {
    return(the$na_interval_set)
  }
  if (is_empty(phint)) {
    return(empty_result(empty_to))
  }
  # <phinterval> NA values are represented as NULL elements in the underlying
  # list vector. rbind() here removes all NULL elements and returns a matrix,
  # except for when every element is NULL, in which case NULL is returned.
  interval_set <- do.call(rbind, vec_data(phint))
  if (is.null(interval_set)) {
    return(the$na_interval_set)
  }
  cpp_squash_interval_set(interval_set)
}

int_to_interval_set <- function(int, na.rm = TRUE, empty_to = "") {
  if (is_empty(int)) {
    return(empty_result(empty_to))
  }
  cpp_squash_lubridate_interval(
    starts = lubridate::int_start(int),
    spans = lubridate::int_length(int),
    na_rm = na.rm
  )
}

empty_result <- function(empty_to) {
  switch(
    empty_to,
    na = the$na_interval_set,
    empty = list(),
    hole = the$empty_interval_set,
    rlang::abort("Unexpected `empty_to`.", .internal = TRUE)
  )
}

# overlaps ---------------------------------------------------------------------

#' Test whether two phintervals overlap
#'
#' @description
#'
#' `phint_overlaps()` tests whether the i-th element of `phint1` overlaps
#' with the i-th element of `phint2`, returning a logical vector. Adjacent
#' intervals are considered overlapping. `phint1` and `phint2` are recycled to
#' their common length using vctrs-style recycling rules.
#'
#' @inheritParams params
#' @return A logical vector.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
#' friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#' mon_and_fri <- phint_union(monday, friday)
#'
#' phint_overlaps(c(monday, monday, friday), c(mon_and_fri, friday, NA))
#'
#' # Adjacent intervals are considered overlapping
#' phint_overlaps(monday, tuesday)
#'
#' # Holes are always considered non-overlapping
#' hole <- phinterval(interval())
#' phint_overlaps(c(hole, monday), c(hole, hole))
#'
#' @export
phint_overlaps <- function(phint1, phint2) {
  phints <- validate_phints(phint1, phint2)
  cpp_interval_sets_overlaps(phint_data(phints[[1]]), phint_data(phints[[2]]))
}

# within -----------------------------------------------------------------------

#' Test whether a date, time, or phinterval is within another phinterval
#'
#' @description
#'
#' `phint_within()` tests whether the i-th element of `x` is contained within
#' the i-th element of `phint`, returning a logical vector. `x` may be a date or
#' time, while `phint` must be a phinterval. `x` and `phint` are recycled to
#' their common length using vctrs-style recycling rules.
#'
#' Dates and times on an endpoint of an interval are considered to be within
#' the interval. An interval is considered to be within itself.
#'
#' @inheritParams params
#'
#' @param x
#'
#' A `<POSIXct>`, `<POSIXlt>`, `<Date>`, `<Interval>` or `<phinterval>`
#' vector to test.
#'
#' @return A logical vector.
#'
#' @examples
#' jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
#' jan_2_to_4 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
#' jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
#'
#' phint_within(
#'   c(jan_2_to_4, jan_3_to_9, jan_1_to_5),
#'   c(jan_1_to_5, jan_1_to_5, NA)
#' )
#'
#' phint_within(as.Date(c("2000-01-06", "2000-01-20")), jan_3_to_9)
#'
#' # Intervals are within themselves
#' phint_within(jan_1_to_5, jan_1_to_5)
#'
#' # Interval endpoints are considered to be within the interval
#' phint_within(as.Date("2000-01-01"), jan_1_to_5)
#'
#' # Holes are never considered to be within an interval
#' hole <- phinterval(interval())
#' phint_within(c(hole, hole), c(hole, jan_1_to_5))
#'
#' @export
phint_within <- function(x, phint) {
  if (lubridate::is.instant(x)) {
    check_is_phintish(phint)
    check_recycleable(x, phint)
    objs <- vctrs::vec_recycle_common(phint, x)
    cpp_interval_sets_contains(
      phint_data(objs[[1]]),
      as.numeric(lubridate::as_datetime(objs[[2]]))
    )
  } else {
    phints <- validate_phints(x, phint)
    cpp_interval_sets_within(phint_data(phints[[1]]), phint_data(phints[[2]]))
  }
}

# utils ------------------------------------------------------------------------

#nocov start
validate_phints <- function(phint1, phint2, call = caller_env()) {
  check_is_phintish(phint1, call = call)
  check_is_phintish(phint2, call = call)
  check_recycleable(phint1, phint2, call = call)
  vctrs::vec_recycle_common(phint1, phint2)
}

phint_data <- function(phint) vec_data(as_phinterval(phint))
#nocov end
