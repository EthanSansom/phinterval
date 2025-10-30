#' @export
phint_squash <- function(phint, na.rm = TRUE) {
  UseMethod("phint_squash")
}

#' @export
phint_squash.phinterval <- function(phint, na.rm = TRUE) {
  stop_wrong_class(na.rm, "logical", n = 1L)

  if (!na.rm && anyNA(phint)) {
    return(na_phinterval(tzone = get_tzone(phint)))
  }

  # <phinterval> NA values are represented as NULL elements in the underlying
  # list vector. rbind() here removes all NULL elements and returns a matrix,
  # except for when every element is NULL, in which case NULL is returned.
  interval_set <- do.call(rbind, vec_data(phint))
  if (is.null(interval_set)) {
    return(na_phinterval(tzone = get_tzone(phint)))
  }

  new_phinterval(
    interval_sets = cpp_squash_interval_set(interval_set),
    tzone = attr(phint, "tzone")
  )
}

#' @export
phint_squash.Interval <- function(phint, na.rm = TRUE) {
  stop_wrong_class(na.rm, "logical", n = 1L)
  new_phinterval(
    interval_sets = int_squash(phint),
    tzone = attr(phint, "tzone")
  )
}

int_squash <- function(int, na.rm = TRUE) {
  starts <- lubridate::int_start(int)
  spans <- lubridate::int_length(int)
  cpp_squash_lubridate_interval(starts, spans, na_rm = na.rm)
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
