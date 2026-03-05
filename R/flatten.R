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
phint_flatten <- function(phint, what = c("spans", "holes")) {
  what <- arg_match(what, c("spans", "holes"))

  squashed <- phint_squash(phint)
  if (is.na(squashed)) {
    return(phinterval(tzone = get_tzone(phint)))
  }
  if (what == "holes") {
    squashed <- phint_invert(squashed)
  }

  unnested <- phint_unnest(squashed)
  phinterval(unnested$start, unnested$end)
}
