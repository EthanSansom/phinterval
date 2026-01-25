' Get the gaps in a phinterval as time spans
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
  out <- phint_unary_dispatch(
    x = phint,
    x_type = validate_type_phintish(phint),
    funs_cpp = list(
      phint = phint_invert_cpp,
      intvl = intvl_invert_cpp
    ),
    hole_to = arg_match(hole_to)
  )
  new_phinterval_bare(out, tzone = get_tzone(phint))
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
