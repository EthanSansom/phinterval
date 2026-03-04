#' Get the gaps in a phinterval as time spans
#'
#' @description
#'
#' `phint_invert()` returns the gaps within a phinterval as a `<phinterval>` vector.
#' For phintervals with multiple disjoint spans, the gaps between those spans are
#' returned. Contiguous time spans (e.g., [lubridate::interval()] vectors) have no
#' gaps and are inverted to holes.
#'
#' `phint_invert()` is similar to `phint_complement()`, except that time occurring
#' outside the extent of `phint` (before its earliest start or after its latest
#' end) is not included in the result.
#'
#' @inheritParams params
#'
#' @param hole_to `["hole" / "inf" / "na"]`
#'
#' How to handle holes (empty phinterval elements):
#' - `"hole"` (default): Holes remain as holes
#' - `"inf"`: Return a span from `-Inf` to `Inf` (all time)
#' - `"na"`: Return an `NA` phinterval
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
#' # Contiguous intervals have no gaps (inverted to holes)
#' phint_invert(monday)
#'
#' # Disjoint intervals: gaps between spans are returned
#' phint_invert(phint_squash(c(monday, friday, sunday)))
#'
#' # The gap between Monday and Friday is Tuesday through Thursday
#' tues_to_thurs <- interval(as.Date("2025-11-11"), as.Date("2025-11-14"))
#' phint_invert(phint_union(monday, friday)) == tues_to_thurs
#'
#' # Invert vs complement: time before and after is excluded from invert
#' mon_and_fri <- phint_union(monday, friday)
#' phint_invert(mon_and_fri)
#' phint_complement(mon_and_fri)
#'
#' # How to invert holes
#' hole <- phint_intersect(monday, friday)
#' phint_invert(hole, hole_to = "hole")
#' phint_invert(hole, hole_to = "inf")
#' phint_invert(hole, hole_to = "na")
#'
#' @export
phint_invert <- function(phint, hole_to = c("hole", "inf", "na")) {
  out <- phint_unary_dispatch(
    x = phint,
    x_type = validate_type_phintish(phint),
    funs_cpp = list(
      phint = phint_invert_cpp,
      intvl = intvl_invert_cpp
    ),
    hole_to = arg_match0(hole_to, c("hole", "inf", "na"))
  )
  new_phinterval_bare(out, tzone = get_tzone(phint))
}

#' Remove instantaneous time spans from a phinterval
#'
#' @description
#'
#' `phint_sift()` removes instantaneous spans (spans with 0 duration) from
#' phinterval elements. If all spans in an element are instantaneous, the result
#' is a hole.
#'
#' @inheritParams params
#'
#' @return A `<phinterval>` vector the same length as `phint`.
#'
#' @examples
#' y2020 <- interval(as.Date("2020-01-01"), as.Date("2021-01-01"))
#' y2021 <- interval(as.Date("2021-01-01"), as.Date("2022-01-01"))
#' y2022 <- interval(as.Date("2022-01-01"), as.Date("2023-01-01"))
#'
#' # The intersection of two adjacent intervals is instantaneous
#' new_years_2021 <- phint_intersect(y2020, y2021)
#' new_years_2021
#' phint_sift(new_years_2021)
#'
#' # phint_sift() removes instants while keeping non-instantaneous spans
#' y2022_and_new_years <- phint_union(y2022, new_years_2021)
#' y2022_and_new_years
#' phint_sift(y2022_and_new_years)
#'
#' @export
phint_sift <- function(phint) {
  out <- phint_unary_dispatch(
    x = phint,
    x_type = validate_type_phintish(phint),
    funs_cpp = list(
      phint = phint_sift_cpp,
      intvl = intvl_sift_cpp
    )
  )
  new_phinterval_bare(out, tzone = get_tzone(phint))
}
