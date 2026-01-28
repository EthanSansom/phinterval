#' Test whether two phintervals overlap
#'
#' @description
#'
#' `phint_overlaps()` tests whether the i-th element of `phint1` overlaps with
#' the i-th element of `phint2`, returning a logical vector. Adjacent intervals
#' (where one ends exactly when the other begins) are considered overlapping.
#' `phint1` and `phint2` are recycled to their common length using vctrs-style
#' recycling rules.
#'
#' @inheritParams params
#'
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
#' # Holes never overlap with anything (including other holes)
#' hole <- hole()
#' phint_overlaps(c(hole, monday), c(hole, hole))
#'
#' @export
phint_overlaps <- function(phint1, phint2) {
  phint_binary_dispatch(
    x = phint1,
    y = phint2,
    x_type = validate_type_phintish(phint1),
    y_type = validate_type_phintish(phint2),
    funs_cpp = list(
      phint_phint = phint_phint_overlaps_cpp,
      phint_intvl = phint_intvl_overlaps_cpp,
      intvl_phint = intvl_phint_overlaps_cpp,
      intvl_intvl = intvl_intvl_overlaps_cpp
    )
  )
}

#' Test whether a datetime or phinterval is within another phinterval
#'
#' @description
#'
#' `phint_within()` tests whether the i-th element of `x` is contained within
#' the i-th element of `phint`, returning a logical vector. `x` may be a datetime
#' (Date, POSIXct, POSIXlt), [lubridate::interval()], or [phinterval()], while
#' `phint` must be a [lubridate::interval()] or [phinterval()]. `x` and `phint`
#' are recycled to their common length using vctrs-style recycling rules.
#'
#' Datetimes on an endpoint of an interval are considered to be within the
#' interval. An interval is considered to be within itself.
#'
#' @inheritParams params
#'
#' @param x `[phinterval / Interval / Date / POSIXct / POSIXlt]`
#'
#' The object to test.
#'
#' @return A logical vector.
#'
#' @examples
#' jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
#' jan_2_to_4 <- interval(as.Date("2000-01-02"), as.Date("2000-01-04"))
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
#' # Interval endpoints are considered within
#' phint_within(as.Date("2000-01-01"), jan_1_to_5)
#'
#' # Holes are never within any interval (including other holes)
#' hole <- hole()
#' phint_within(c(hole, hole), c(hole, jan_1_to_5))
#'
#' @export
phint_within <- function(x, phint) {
  phint_binary_dispatch(
    x = x,
    y = phint,
    x_type = validate_type_phintish_or_instant(x),
    y_type = validate_type_phintish(phint),
    funs_cpp = list(
      phint_phint = phint_phint_within_cpp,
      phint_intvl = phint_intvl_within_cpp,
      intvl_phint = intvl_phint_within_cpp,
      intvl_intvl = intvl_intvl_within_cpp,
      point_phint = point_phint_within_cpp,
      point_intvl = point_intvl_within_cpp
    )
  )
}
