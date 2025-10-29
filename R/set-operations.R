#' @export
phint_squash <- function(phint, na.rm = TRUE, empty = c("na", "empty", "hole")) {

  stop_wrong_class(na.rm, "logical", n = 1L)
  empty <- rlang::arg_match(empty)
  phint <- check_is_phinty(phint)
  tzone <- tz(phint)

  if (rlang::is_empty(phint)) {
    return(switch(
      empty,
      "empty" = phinterval(tzone = tzone),
      "na" = na_phinterval(tzone = tzone),
      "hole" = hole_phinterval(tzone = tzone)
    ))
  }
  if (lubridate::is.interval(phint)) {
    return(int_squash(phint, na.rm = na.rm))
  }

  reference_seconds <- as.double(field(phint, "reference_time"))
  range_starts <- field(phint, "range_starts") |> map2(reference_seconds, `+`)
  range_ends <- field(phint, "range_ends") |> map2(reference_seconds, `+`)

  na_at <- is.na(reference_seconds)
  if (all(na_at) || (any(na_at) && !na.rm)) {
    return(na_phinterval(tzone = tzone))
  }
  if (na.rm) {
    range_starts <- range_starts[!na_at]
    range_ends <- range_ends[!na_at]
  }

  flat_ranges <- range_flatten(list_c(range_starts), list_c(range_ends))
  new_phinterval(
    reference_time = origin_posixct(1L, tzone = tzone),
    range_starts = list(flat_ranges$starts),
    range_ends = list(flat_ranges$ends),
    tzone = tzone
  )

}

#' @export
int_squash <- function(int, na.rm = TRUE) {

  int <- lubridate::int_standardize(int)
  int_starts <- lubridate::int_start(int)
  int_ends <- lubridate::int_end(int)
  tzone <- lubridate::tz(int_starts)

  na_at <- is.na(int)
  if (all(na_at) || (any(na_at) && !na.rm)) {
    return(na_phinterval(tzone = tzone))
  }
  if (na.rm) {
    int_starts <- int_starts[!na_at]
    int_ends <- int_ends[!na_at]
  }

  flat_ranges <- range_flatten(as.double(int_starts), as.double(int_ends))
  new_phinterval(
    reference_time = origin_posixct(1L, tzone = tzone),
    range_starts = list(flat_ranges$starts),
    range_ends = list(flat_ranges$ends),
    tzone = tzone
  )
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
