# TODO: Document
#' @export
phint_squash <- function(x, by = NULL, na.rm = TRUE) {
  x_type <- validate_type_phintish(x)
  if (!is.null(by)) {
    check_vector(by)
    check_recycleable_to(by, x)
  }
  check_bool(na.rm)

  if (is.null(by) || vec_size(by) == 1L) {
    out <- phint_unary_dispatch(
      x = x,
      x_type = x_type,
      funs_cpp = list(
        phint = phint_squash_cpp,
        intvl = intvl_squash_cpp
      ),
      na_rm = na.rm
    )
  } else {
    groups <- vec_group_loc(by)
    out <- phint_unary_dispatch(
      x = x,
      x_type = x_type,
      funs_cpp = list(
        phint = phint_squash_cpp,
        intvl = intvl_squash_cpp
      ),
      na_rm = na.rm,
      group_locs = groups[["loc"]]
    )
  }

  new_phinterval_bare(out, tzone = get_tzone(x))
}

# TODO: Document
#' @export
datetime_squash <- function(start, end, by = NULL, na.rm = FALSE) {
  check_instant(start)
  check_instant(end)
  check_recycleable(start, end)
  check_bool(na.rm)
  check_vector(by, allow_null = TRUE)

  range <- vec_recycle_common(starts = as.POSIXct(start), ends = as.POSIXct(end))
  if (!is_null(by)) {
    check_recycleable_to(
      x = by,
      to = range$starts,
      to_arg = "vctrs::vec_recycle_common(start, end)"
    )
  }

  if (is.null(by) || vec_size(by) == 1L) {
    out <- range_squash_cpp(
      starts = range$starts,
      ends = range$ends,
      na_rm = na.rm
    )
  } else {
    groups <- vec_group_loc(by)
    out <- range_squash_by_cpp(
      starts = range$starts,
      ends = range$ends,
      group_locs = groups[["loc"]],
      na_rm = na.rm
    )
  }

  new_phinterval_bare(out, tzone = tz_union(start, end))
}
