#' Flatten a phinterval vector into a vector of spans or gaps
#'
#' @description
#' `phint_flatten()` collapses all elements of `phint` into a single set of
#' non-overlapping time spans, then returns them as a flat `<phinterval>` vector
#' with one span per element. `NA` elements are ignored.
#'
#' - `what = "spans"` (default): returns the time spans covered by any element
#'   of `phint`.
#' - `what = "holes"`: returns the gaps between those spans.
#'
#' @inheritParams params
#'
#' @param what `["spans" / "holes"]`
#'
#' Whether to return the covered spans or the intervening gaps:
#' - `"spans"` (default): Time spans covered by at least one element of `phint`.
#' - `"holes"`: Gaps between covered spans (excludes the infinite extents before
#'   the first span and after the last span).
#'
#' @return
#'
#' A `<phinterval>` vector with the invariant `all(n_spans(phint) == 1L)`.
#'
#' @examples
#' monday <- interval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' tuesday <- interval(as.Date("2025-11-11"), as.Date("2025-11-12"))
#' friday <- interval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#' thurs_and_sat <- phint_union(
#'   interval(as.Date("2025-11-13"), as.Date("2025-11-14")),
#'   interval(as.Date("2025-11-15"), as.Date("2025-11-16"))
#' )
#' noon_wednesday <- as_phinterval(as.POSIXct("2025-11-12 12:00:00"))
#'
#' # Flatten into individual spans
#' phint_flatten(c(monday, thurs_and_sat))
#'
#' # Flatten into gaps between spans
#' phint_flatten(c(monday, thurs_and_sat), what = "holes")
#' phint_flatten(thurs_and_sat, what = "holes") == friday
#'
#' # Overlapping or adjacent elements are merged before flattening
#' phint_flatten(c(monday, tuesday, friday))
#'
#' # NA elements are ignored
#' phint_flatten(c(monday, NA, friday))
#' phint_flatten(interval(NA, NA))
#'
#' # Instants are preserved when flattening into spans
#' phint_flatten(c(monday, noon_wednesday, friday), what = "spans")
#'
#' # Instants between two spans are ignored when flattening into gaps
#' phint_flatten(c(monday, noon_wednesday, friday), what = "holes")
#'
#' @name flatten
NULL

#' @rdname flatten
#' @export
phint_flatten <- function(phint, what = c("spans", "holes")) {
  out <- phint_unary_dispatch(
    x = phint,
    x_type = validate_type_phintish(phint),
    funs_cpp = list(
      phint = phint_flatten_cpp,
      intvl = intvl_flatten_cpp
    ),
    what = arg_match(what, c("spans", "holes"))
  )
  new_phinterval_bare(out, tzone = get_tzone(phint))
}

#' @rdname flatten
#' @export
datetime_flatten <- function(start, end, what = c("spans", "holes")) {
  check_instant(start)
  check_instant(end)
  check_recycleable(start, end)
  what <- arg_match(what, c("spans", "holes"))

  if (length(starts) == length(ends)) {
    out <- range_flatten_cpp(
      starts = start,
      ends = end,
      what = what
    )
  } else {
    range <- vec_recycle_common(start = start, end = end)
    out <- range_flatten_cpp(
      starts = range$start,
      ends = range$end,
      what = what
    )
  }
  new_phinterval_bare(out, tzone = tz_union(start, end))
}
