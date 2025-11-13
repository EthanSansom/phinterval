# squash -----------------------------------------------------------------------

#' @export
phint_squash <- function(phint, na.rm = TRUE) {
  UseMethod("phint_squash")
}

#' @export
phint_squash.default <- function(phint, na.rm = TRUE) {
  check_is_phintish(phint)
}

#' @export
phint_squash.phinterval <- function(phint, na.rm = TRUE) {
  check_bool(na.rm)
  new_phinterval(
    interval_sets = phint_to_interval_set(phint, na.rm = na.rm),
    tzone = attr(phint, "tzone")
  )
}

#' @export
phint_squash.Interval <- function(phint, na.rm = TRUE) {
  check_bool(na.rm)
  new_phinterval(
    interval_sets = int_to_interval_set(phint, na.rm = na.rm),
    tzone = attr(phint, "tzone")
  )
}

phint_to_interval_set <- function(phint, na.rm = TRUE) {
  if (!na.rm && anyNA(phint)) {
    return(NULL)
  }
  # <phinterval> NA values are represented as NULL elements in the underlying
  # list vector. rbind() here removes all NULL elements and returns a matrix,
  # except for when every element is NULL, in which case NULL is returned.
  interval_set <- do.call(rbind, vec_data(phint))
  if (is.null(interval_set)) {
    return(NULL)
  }
  cpp_squash_interval_set(interval_set)
}

int_to_interval_set <- function(int, na.rm = TRUE) {
  cpp_squash_lubridate_interval(
    starts = lubridate::int_start(int),
    spans = lubridate::int_length(int),
    na_rm = na.rm
  )
}

# overlaps ---------------------------------------------------------------------

#' @export
phint_overlaps <- function(phint1, phint2) {
  check_is_phintish(phint1)
  check_is_phintish(phint2)
  check_recycleable(phint1, phint2)
  phints <- vctrs::vec_recycle_common(phint1, phint2)

  new_phinterval(
    cpp_interval_sets_overlaps(
      vec_data(as_phinterval(phints[[1]])),
      vec_data(as_phinterval(phints[[2]]))
    ),
    tzone = tz_union(phint1, phint2)
  )
}

# union ------------------------------------------------------------------------

#' @export
phint_union <- function(phint1, phint2) {
  check_is_phintish(phint1)
  check_is_phintish(phint2)
  check_recycleable(phint1, phint2)
  phints <- vctrs::vec_recycle_common(phint1, phint2)

  new_phinterval(
    cpp_union_interval_sets(
      vec_data(as_phinterval(phints[[1]])),
      vec_data(as_phinterval(phints[[2]]))
    ),
    tzone = tz_union(phint1, phint2)
  )
}

# complement -------------------------------------------------------------------

#' @export
phint_complement <- function(phint) {
  check_is_phintish(phint)

  new_phinterval(
    cpp_complement_interval_sets(vec_data(as_phinterval(phint))),
    tzone = get_tzone(phint)
  )
}

# intersection -----------------------------------------------------------------

#' @export
phint_intersect <- function(phint1, phint2) {
  check_is_phintish(phint1)
  check_is_phintish(phint2)
  check_recycleable(phint1, phint2)
  phints <- vctrs::vec_recycle_common(phint1, phint2)

  new_phinterval(
    cpp_intersect_interval_sets(
      vec_data(as_phinterval(phints[[1]])),
      vec_data(as_phinterval(phints[[2]]))
    ),
    tzone = tz_union(phint1, phint2)
  )
}

# difference -------------------------------------------------------------------

#' @export
phint_setdiff <- function(phint1, phint2) {
  check_is_phintish(phint1)
  check_is_phintish(phint2)
  check_recycleable(phint1, phint2)
  phints <- vctrs::vec_recycle_common(phint1, phint2)

  new_phinterval(
    cpp_setdiff_interval_sets(
      vec_data(as_phinterval(phints[[1]])),
      vec_data(as_phinterval(phints[[2]]))
    ),
    tzone = tz_union(phint1, phint2)
  )
}
