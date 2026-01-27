# TODO: Document
#' @export
phint_squash <- function(
    x,
    by = NULL,
    na.rm = TRUE,
    empty_to = c("hole", "na", "empty"),
    order_by = FALSE
) {
  x_type <- validate_type_phintish(x)
  if (!is.null(by)) {
    check_vector(by)
    check_recycleable_to(by, x)
  }
  check_bool(na.rm)
  empty_to <- arg_match0(empty_to, c("hole", "na", "empty"))
  check_bool(order_by)

  if (is_empty(x)) {
    return(empty_squash(empty_to, tzone = get_tzone(x)))
  }

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
    groups <- if (order_by) vec_locate_sorted_groups(by) else vec_group_loc(by)
    out <- phint_unary_dispatch(
      x = x,
      x_type = x_type,
      funs_cpp = list(
        phint = phint_squash_by_cpp,
        intvl = intvl_squash_by_cpp
      ),
      na_rm = na.rm,
      group_locs = groups[["loc"]]
    )
  }

  new_phinterval_bare(out, tzone = get_tzone(x))
}

# TODO: Document
#' @export
datetime_squash <- function(
    start,
    end,
    by = NULL,
    na.rm = TRUE,
    empty_to = c("hole", "na", "empty"),
    order_by = FALSE
) {
  check_instant(start)
  check_instant(end)
  check_recycleable(start, end)
  check_vector(by, allow_null = TRUE)
  check_bool(na.rm)
  empty_to <- arg_match0(empty_to, c("hole", "na", "empty"))
  check_bool(order_by)

  range <- vec_recycle_common(starts = as.POSIXct(start), ends = as.POSIXct(end))
  if (!is_null(by)) {
    check_recycleable_to(
      x = by,
      to = range$starts,
      to_arg = "vctrs::vec_recycle_common(start, end)"
    )
  }

  datetime_squash_impl(
    starts = range$starts,
    ends = range$ends,
    by = by,
    tzone = tz_union(start, end),
    na.rm = na.rm,
    empty_to = empty_to,
    order_by = order_by
  )
}

datetime_squash_impl <- function(starts, ends, by, tzone, na.rm, empty_to, order_by) {
  if (is_empty(starts)) {
    return(empty_squash(empty_to, tzone = tz_union(starts, ends)))
  }

  if (vec_size(by) == 1L || is.null(by)) {
    out <- range_squash_cpp(
      starts = starts,
      ends = ends,
      na_rm = na.rm
    )
  } else {
    groups <- if (order_by) vec_locate_sorted_groups(by) else vec_group_loc(by)
    out <- range_squash_by_cpp(
      starts = starts,
      ends = ends,
      group_locs = groups[["loc"]],
      na_rm = na.rm
    )
  }

  new_phinterval_bare(out, tzone = tzone)
}

empty_squash <- function(empty_to, tzone) {
  switch(
    empty_to,
    hole = hole(tzone = tzone),
    na = na_phinterval(tzone = tzone),
    empty = phinterval(tzone = tzone)
  )
}
