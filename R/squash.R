#' Squash overlapping intervals into non-overlapping spans
#'
#' @description
#'
#' `phint_squash()` and `datetime_squash()` merge overlapping or adjacent
#' intervals into a minimal set of non-overlapping, non-adjacent time spans.
#'
#' - `phint_squash()` takes a `<phinterval>` or `<Interval>` vector
#' - `datetime_squash()` takes separate `start` and `end` datetime vectors
#'
#' When `by = NULL` (the default), all intervals are merged into a single
#' phinterval element. When `by` is provided, intervals are grouped and merged
#' separately within each group, creating one phinterval element per unique
#' value of `by`.
#'
#' @details
#'
#' These functions are particularly useful in aggregation workflows with
#' [dplyr::summarize()] to combine intervals within groups.
#'
#' @inheritParams params
#'
#' @param start `[POSIXct / POSIXlt / Date]`
#'
#' A vector of start times. Must be recyclable with `end`. Only used in
#' `datetime_squash()`.
#'
#' @param end `[POSIXct / POSIXlt / Date]`
#'
#' A vector of end times. Must be recyclable with `start`. Only used in
#' `datetime_squash()`.
#'
#' @param by `[vector / data.frame / NULL]`
#'
#' An optional grouping vector or data frame. When provided, intervals are
#' grouped by `by` and merged separately within each group. If `NULL` (the
#' default), all intervals are merged into a single phinterval element.
#'
#' For `datetime_squash()`, `by` must be recyclable with the recycled length
#' of `start` and `end`.
#'
#' `by` may be any vector in the vctrs sense. See [vctrs::obj_is_vector()]
#' for details.
#'
#' @param na.rm `[TRUE / FALSE]`
#'
#' Should `NA` elements be removed before squashing? If `FALSE` and any `NA`
#' elements are present, the result for that group is `NA`. Defaults to `TRUE`.
#'
#' @param empty_to `["hole" / "na" / "empty"]`
#'
#' How to handle empty inputs (length-0 vectors or groups with only `NA` values
#' when `na.rm = TRUE`):
#' - `"hole"` (default): Return a hole
#' - `"na"`: Return an `NA` phinterval
#' - `"empty"`: Return a length-0 phinterval vector
#'
#' @param order_by `[TRUE / FALSE]`
#'
#' Should the output be ordered by the values in `by`? If `FALSE` (the default),
#' the output order matches the first appearance of each group in `by`. If `TRUE`,
#' the output is sorted by the unique values of `by`. Only used when `by` is not
#' `NULL`.
#'
#' @param keep_by `[TRUE / FALSE]`
#'
#' Should the `by` values be returned alongside the result? If `FALSE` (the
#' default), returns a `<phinterval>` vector. If `TRUE`, returns a [tibble::tibble()]
#' with columns `by` and `phint`. Requires `by` to be non-`NULL`.
#'
#' @return
#'
#' When `keep_by = FALSE`:
#' - If `by = NULL`: A length-1 `<phinterval>` vector (or length-0 if the input
#'   is empty and `empty_to = "empty"`)
#' - If `by` is provided: A `<phinterval>` vector with one element per unique
#'   value of `by`
#'
#' When `keep_by = TRUE`: A [tibble::tibble()] with columns `by` and `phint`.
#'
#' @examples
#' jan_1_to_5 <- interval(as.Date("2000-01-01"), as.Date("2000-01-05"))
#' jan_3_to_9 <- interval(as.Date("2000-01-03"), as.Date("2000-01-09"))
#' jan_11_to_12 <- interval(as.Date("2000-01-11"), as.Date("2000-01-12"))
#'
#' # phint_squash: merge intervals from a phinterval/Interval vector
#' phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12))
#'
#' # datetime_squash: merge intervals from start/end vectors
#' datetime_squash(
#'   start = as.Date(c("2000-01-01", "2000-01-03", "2000-01-11")),
#'   end = as.Date(c("2000-01-05", "2000-01-09", "2000-01-12"))
#' )
#'
#' # NA values are removed by default
#' phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA))
#'
#' # Set na.rm = FALSE to propagate NA values
#' phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA), na.rm = FALSE)
#'
#' # Squash within groups
#' phint_squash(
#'   c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
#'   by = c(1, 1, 2)
#' )
#'
#' # Return a data frame with by values
#' phint_squash(
#'   c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
#'   by = c("A", "A", "B"),
#'   keep_by = TRUE
#' )
#'
#' # Control output order with order_by
#' phint_squash(
#'   c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
#'   by = c(2, 2, 1),
#'   order_by = TRUE
#' )
#'
#' # empty_to determines the result of empty inputs
#' empty <- phinterval()
#' phint_squash(empty, empty_to = "hole")
#' phint_squash(empty, empty_to = "na")
#' phint_squash(empty, empty_to = "empty")
#'
#' @name squash
NULL

#' @rdname squash
#' @export
phint_squash <- function(
    phint,
    by = NULL,
    na.rm = TRUE,
    empty_to = c("hole", "na", "empty"),
    order_by = FALSE,
    keep_by = FALSE
) {
  phint_type <- validate_type_phintish(phint)
  by_is_null <- is.null(by)
  if (!by_is_null) {
    check_vector(by)
    check_recycleable_to(by, phint)
  }
  check_bool(na.rm)
  empty_to <- arg_match0(empty_to, c("hole", "na", "empty"))
  check_bool(order_by)
  check_bool(keep_by)

  if (keep_by && by_is_null) {
    abort("Can't use `keep_by = TRUE` when `by = NULL`.")
  }

  if (is_empty(phint)) {
    return(empty_squash(empty_to, tzone = get_tzone(phint), by = by, keep_by = keep_by))
  }

  if (by_is_null || vec_size(by) == 1L) {
    groups <- data.frame(key = by)
    out <- phint_unary_dispatch(
      x = phint,
      x_type = phint_type,
      funs_cpp = list(
        phint = phint_squash_cpp,
        intvl = intvl_squash_cpp
      ),
      na_rm = na.rm
    )
  } else {
    groups <- if (order_by) vec_locate_sorted_groups(by) else vec_group_loc(by)
    out <- phint_unary_dispatch(
      x = phint,
      x_type = phint_type,
      funs_cpp = list(
        phint = phint_squash_by_cpp,
        intvl = intvl_squash_by_cpp
      ),
      na_rm = na.rm,
      group_locs = .subset2(groups, "loc")
    )
  }

  if (keep_by) {
    tibble::new_tibble(list(
      by = groups$key,
      phint = new_phinterval_bare(out, tzone = get_tzone(phint))
    ))
  } else {
    new_phinterval_bare(out, tzone = get_tzone(phint))
  }
}

#' @rdname squash
#' @export
datetime_squash <- function(
    start,
    end,
    by = NULL,
    na.rm = TRUE,
    empty_to = c("hole", "na", "empty"),
    order_by = FALSE,
    keep_by = FALSE
) {
  check_instant(start)
  check_instant(end)
  check_recycleable(start, end)
  check_vector(by, allow_null = TRUE)
  check_bool(na.rm)
  empty_to <- arg_match0(empty_to, c("hole", "na", "empty"))
  check_bool(order_by)
  check_bool(keep_by)

  if (keep_by && is.null(by)) {
    abort("`keep_by = TRUE` requires a `by` argument.")
  }

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
    order_by = order_by,
    keep_by = keep_by
  )
}

datetime_squash_impl <- function(starts, ends, by, tzone, na.rm, empty_to, order_by, keep_by) {
  if (is_empty(starts)) {
    return(empty_squash(empty_to, tzone = tzone, by = by, keep_by = keep_by))
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
      group_locs = .subset2(groups, "loc"),
      na_rm = na.rm
    )
  }

  if (keep_by) {
    tibble::new_tibble(list(
      by = .subset2(groups, "key"),
      phint = new_phinterval_bare(out, tzone = tzone)
    ))
  } else {
    new_phinterval_bare(out, tzone = tzone)
  }
}

# helpers ----------------------------------------------------------------------

empty_squash <- function(empty_to, tzone, by = NULL, keep_by = FALSE) {
  phint <- switch(
    empty_to,
    hole = hole(tzone = tzone),
    na = na_phinterval(tzone = tzone),
    empty = phinterval(tzone = tzone)
  )

  if (keep_by) {
    # vctrs-recycling was enforced prior to this point, so `by` is either a
    # length-0 or length-1 vector. If `by` is empty but `phint` is length-1,
    # initializing an `NA` vector using `by` as the type template.
    if (vec_is_empty(by)) {
      by <- if (empty_to == "empty") by else vec_init(by, 1L)
    } else {
      by <- if (empty_to == "empty") vec_slice(by, 0) else by
    }
    return(tibble::new_tibble(list(by = by, phint = phint)))
  }

  phint
}
