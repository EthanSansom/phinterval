# todos ------------------------------------------------------------------------

# TODO: Move to C++
# - These are mock-ups in R, that'll be moved to an Rcpp implementation later

# TODO: Maybe allow the direction to be reversed (e.g. the `right` option of Reduce)

# set operations ---------------------------------------------------------------

phint_cumunion <- function(phint, na_propogate = FALSE, reverse = FALSE) {
  check_phintish(phint)
  check_bool(na_propogate)
  check_bool(reverse)

  if (!na_propogate) {
    phint[is.na(phint)] <- hole(tzone = get_tzone(phint))
  }

  phint_accumulate(phint, phint_union, reverse = reverse)
}

# helpers ----------------------------------------------------------------------

# The standalone-purrr accumulation attempts to simplify, unlists the phinterval
phint_accumulate <- function(.x, .f, ..., reverse = FALSE) {
  f <- function(x, y) .f(x, y, ...)
  vctrs::list_unchop(Reduce(f, .x, accumulate = TRUE, right = reverse, simplify = FALSE))
}
