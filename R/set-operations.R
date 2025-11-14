# complement -------------------------------------------------------------------

#' @export
phint_complement <- function(phint) {
  check_is_phintish(phint)
  new_phinterval(
    cpp_complement_interval_sets(phint_data(phint)),
    tzone = get_tzone(phint)
  )
}

# union ------------------------------------------------------------------------

#' @export
phint_union <- function(phint1, phint2) {
  phints <- validate_phints(phint1, phint2)
  new_phinterval(
    cpp_union_interval_sets(phint_data(phints[[1]]), phint_data(phints[[2]])),
    tzone = tz_union(phint1, phint2)
  )
}

# intersection -----------------------------------------------------------------

#' @export
phint_intersect <- function(phint1, phint2) {
  phints <- validate_phints(phint1, phint2)
  new_phinterval(
    cpp_intersect_interval_sets(phint_data(phints[[1]]), phint_data(phints[[2]])),
    tzone = tz_union(phint1, phint2)
  )
}

# difference -------------------------------------------------------------------

#' @export
phint_setdiff <- function(phint1, phint2) {
  phints <- validate_phints(phint1, phint2)
  new_phinterval(
    cpp_setdiff_interval_sets(phint_data(phints[[1]]), phint_data(phints[[2]])),
    tzone = tz_union(phint1, phint2)
  )
}

# squash -----------------------------------------------------------------------

#' @export
phint_squash <- function(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty")) {
  UseMethod("phint_squash")
}

#' @export
phint_squash.default <- function(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty")) {
  check_is_phintish(phint)
}

#' @export
phint_squash.phinterval <- function(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty")) {
  check_bool(na.rm)
  empty_to <- arg_match(empty_to)
  new_phinterval(
    interval_sets = phint_to_interval_set(phint, na.rm = na.rm, empty_to = empty_to),
    tzone = get_tzone(phint)
  )
}

#' @export
phint_squash.Interval <- function(phint, na.rm = TRUE, empty_to = c("na", "hole", "empty")) {
  check_bool(na.rm)
  empty_to <- arg_match(empty_to)
  new_phinterval(
    interval_sets = int_to_interval_set(phint, na.rm = na.rm, empty_to = empty_to),
    tzone = get_tzone(phint)
  )
}

phint_to_interval_set <- function(phint, na.rm = TRUE, empty_to = "") {
  if (!na.rm && anyNA(phint)) {
    return(the$na_interval_set)
  }
  if (is_empty(phint)) {
    return(empty_result(empty_to))
  }
  # <phinterval> NA values are represented as NULL elements in the underlying
  # list vector. rbind() here removes all NULL elements and returns a matrix,
  # except for when every element is NULL, in which case NULL is returned.
  interval_set <- do.call(rbind, vec_data(phint))
  if (is.null(interval_set)) {
    return(the$na_interval_set)
  }
  cpp_squash_interval_set(interval_set)
}

int_to_interval_set <- function(int, na.rm = TRUE, empty_to = "") {
  if (is_empty(int)) {
    return(empty_result(empty_to))
  }
  cpp_squash_lubridate_interval(
    starts = lubridate::int_start(int),
    spans = lubridate::int_length(int),
    na_rm = na.rm
  )
}

empty_result <- function(empty_to) {
  switch(
    empty_to,
    na = the$na_interval_set,
    empty = list(),
    hole = the$empty_interval_set,
    rlang::abort("Unexpected `empty_to`.", .internal = TRUE)
  )
}

# overlaps ---------------------------------------------------------------------

#' @export
phint_overlaps <- function(phint1, phint2) {
  phints <- validate_phints(phint1, phint2)
  cpp_interval_sets_overlaps(phint_data(phints[[1]]), phint_data(phints[[2]]))
}

# within -----------------------------------------------------------------------

#' @export
phint_within <- function(x, phint) {
  if (lubridate::is.instant(x)) {
    check_is_phintish(phint)
    check_recycleable(x, phint)
    objs <- vctrs::vec_recycle_common(phint, x)
    cpp_interval_sets_contains(
      phint_data(objs[[1]]),
      as.numeric(as.POSIXct(objs[[2]]))
    )
  } else {
    phints <- validate_phints(x, phint)
    cpp_interval_sets_within(phint_data(phints[[1]]), phint_data(phints[[2]]))
  }
}

# utils ------------------------------------------------------------------------

#nocov start
validate_phints <- function(phint1, phint2, call = caller_env()) {
  check_is_phintish(phint1, call = call)
  check_is_phintish(phint2, call = call)
  check_recycleable(phint1, phint2, call = call)
  vctrs::vec_recycle_common(phint1, phint2)
}

phint_data <- function(phint) vec_data(as_phinterval(phint))
#nocov end
