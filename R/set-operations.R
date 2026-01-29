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
#'
#' @param bounds `["[]" / "()"]`
#'
#' For `phint_intersect()` only, whether span endpoints are inclusive or exclusive:
#' - `"[]"` (default): Closed intervals - both endpoints are included
#' - `"()"`: Open intervals - both endpoints are excluded
#'
#' This affects adjacency and overlap detection. For example, with `bounds = "[]"`,
#' the intervals `[1, 5]` and `[5, 10]` are considered adjacent (they share the
#' endpoint 5), while with `bounds = "()"`, `(1, 5)` and `(5, 10)` are disjoint
#' (neither includes 5).
#'
#' @return A `<phinterval>` vector.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
#' friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#' jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
#' jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
#'
#' # Complement
#' phint_complement(jan_1_to_5)
#'
#' # The complement of a hole is an infinite span covering all time
#' hole <- hole()
#' phint_complement(hole)
#'
#' # Union
#' phint_union(c(monday, monday, monday), c(tuesday, friday, NA))
#'
#' # Elements of length 1 are recycled
#' phint_union(monday, c(tuesday, friday, NA))
#'
#' # Intersection
#' phint_intersect(jan_1_to_5, jan_3_to_9)
#'
#' # The intersection of non-overlapping intervals is a hole
#' phint_intersect(monday, friday)
#'
#' # By default, the intersection of adjacent intervals is instantaneous
#' phint_intersect(monday, tuesday)
#'
#' # Use bounds to set the intersection of adjacent intervals to a hole
#' phint_intersect(monday, tuesday, bounds = "()")
#'
#' # Set difference
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
  out <- phint_unary_dispatch(
    x = phint,
    x_type = validate_type_phintish(phint),
    funs_cpp = list(
      phint = phint_complement_cpp,
      intvl = intvl_complement_cpp
    )
  )
  new_phinterval_bare(out, tzone = get_tzone(phint))
}

#' @rdname phinterval-set-operations
#' @export
phint_union <- function(phint1, phint2) {
  out <- phint_binary_dispatch(
    x = phint1,
    y = phint2,
    x_type = validate_type_phintish(phint1),
    y_type = validate_type_phintish(phint2),
    funs_cpp = list(
      phint_phint = phint_phint_union_cpp,
      phint_intvl = phint_intvl_union_cpp,
      intvl_phint = intvl_phint_union_cpp,
      intvl_intvl = intvl_intvl_union_cpp
    )
  )
  new_phinterval_bare(out, tzone = tz_union(phint1, phint2))
}

#' @rdname phinterval-set-operations
#' @export
phint_intersect <- function(phint1, phint2, bounds = c("[]", "()")) {
  out <- phint_binary_dispatch(
    x = phint1,
    y = phint2,
    x_type = validate_type_phintish(phint1),
    y_type = validate_type_phintish(phint2),
    funs_cpp = list(
      phint_phint = phint_phint_intersect_cpp,
      phint_intvl = phint_intvl_intersect_cpp,
      intvl_phint = intvl_phint_intersect_cpp,
      intvl_intvl = intvl_intvl_intersect_cpp
    ),
    bounds = arg_match0(bounds, c("[]", "()"))
  )
  new_phinterval_bare(out, tzone = tz_union(phint1, phint2))
}

#' @rdname phinterval-set-operations
#' @export
phint_setdiff <- function(phint1, phint2) {
  out <- phint_binary_dispatch(
    x = phint1,
    y = phint2,
    x_type = validate_type_phintish(phint1),
    y_type = validate_type_phintish(phint2),
    funs_cpp = list(
      phint_phint = phint_phint_setdiff_cpp,
      phint_intvl = phint_intvl_setdiff_cpp,
      intvl_phint = intvl_phint_setdiff_cpp,
      intvl_intvl = intvl_intvl_setdiff_cpp
    )
  )
  new_phinterval_bare(out, tzone = tz_union(phint1, phint2))
}
