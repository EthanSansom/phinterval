#' Unnest a phinterval into a data frame
#'
#' @description
#'
#' `phint_unnest()` converts a `<phinterval>` vector into a [tibble::tibble()]
#' where each time span becomes a row.
#'
#' @details
#'
#' `phint_unnest()` expands each phinterval element into its constituent time
#' spans, creating one row per span. The resulting data frame contains a `key`
#' column identifying which phinterval element each span came from, a `start` and
#' `end` column for each span's boundaries, and a `size` column counting the
#' number of spans in the phinterval element.
#'
#' For phinterval elements containing multiple disjoint spans, all spans are
#' included with the same `key` value and `size`. Scalar phinterval elements
#' (single spans) produce a single row. Both `NA` elements and [hole()]s produce
#' `NA` values in the `start` and `end` columns, but have a `size` of `NA` and `0`
#' respectively.
#'
#' @param phint `[phinterval / Interval]`
#'
#' A `<phinterval>` or `<Interval>` vector to unnest.
#'
#' @param hole_to `["na" / "drop"]`
#'
#' How to handle hole elements (phintervals with zero spans). If `"na"` (the
#' default), a row with `NA` start and end times and a size of `0` is included
#' for each hole. If `"drop"`, holes are excluded from the output.
#'
#' @param key `[vector / data.frame / NULL]`
#'
#' An optional vector or data frame to use as the `key` column in the output.
#' If provided, must be the same length as `phint`. If `NULL` (the default),
#' the `key` column contains row indices (position in `phint`).
#'
#' `key` may be any vector in the vctrs sense. See [vctrs::obj_is_vector()]
#' for details.
#'
#' @return
#'
#' A [tibble::tibble()] with columns:
#' - `key`:
#'    - If `key = NULL`: A numeric vector identifying the index of the phinterval element.
#'    - Otherwise: The element of `key` corresponding to the phinterval element.
#' - `start`: POSIXct start time of the span.
#' - `end`: POSIXct end time of the span.
#' - `size`: Integer count of spans in the phinterval element.
#'
#' @examples
#' # Unnest scalar phintervals
#' phint <- phinterval(
#'   start = as.Date(c("2000-01-01", "2000-02-01")),
#'   end = as.Date(c("2000-01-15", "2000-02-15"))
#' )
#' phint_unnest(phint)
#'
#' # Unnest multi-span phinterval
#' phint <- phinterval(
#'   start = as.Date(c("2000-01-01", "2000-03-01")),
#'   end = as.Date(c("2000-01-15", "2000-03-15")),
#'   by = 1
#' )
#' phint_unnest(phint)
#'
#' # Handle holes
#' phint <- c(
#'   phinterval(as.Date("2000-01-01"), as.Date("2000-01-15")),
#'   hole(),
#'   phinterval(as.Date("2000-02-01"), as.Date("2000-02-15"))
#' )
#' phint_unnest(phint, hole_to = "na")
#' phint_unnest(phint, hole_to = "drop")
#'
#' # Use a custom `key`
#' phint_unnest(phint, key = c("A", "B", "C"), hole_to = "na")
#'
#' @export
phint_unnest <- function(phint, key = NULL, hole_to = c("na", "drop")) {
  UseMethod("phint_unnest")
}

#' @export
phint_unnest.default <- function(phint, key = NULL, hole_to = c("na", "drop")) {
  check_phintish(phint)
}

#' @export
phint_unnest.Interval <- function(phint, key = NULL, hole_to = c("na", "drop")) {
  hole_to <- arg_match0(hole_to, values = c("drop", "na"))
  if (!is.null(key)) {
    check_vector(key)
    check_same_size(phint, key)
  }

  out <- intvl_unnest_cpp(
    starts = lubridate::int_start(phint),
    spans = lubridate::int_length(phint),
    tzone = get_tzone(phint),
    hole_to = hole_to
  )

  if (!is.null(key)) {
    out[["key"]] <- vec_slice(key, out[["key"]])
  }
  tibble::new_tibble(out)
}

#' @export
phint_unnest.phinterval <- function(phint, key = NULL, hole_to = c("drop", "na")) {
  hole_to <- arg_match0(hole_to, values = c("drop", "na"))
  if (!is.null(key)) {
    check_vector(key)
    check_same_size(phint, key)
  }

  out <- phint_unnest_cpp(
    size = field(phint, "size"),
    starts = field(phint, "starts"),
    ends = field(phint, "ends"),
    tzone = get_tzone(phint),
    hole_to = hole_to
  )

  if (!is.null(key)) {
    out[["key"]] <- vec_slice(key, out[["key"]])
  }
  tibble::new_tibble(out)
}
