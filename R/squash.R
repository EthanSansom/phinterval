#' Squash overlapping intervals into non-overlapping spans
#'
#' @description
#'
#' `phint_squash()` and `datetime_squash()` merge overlapping or adjacent
#' intervals into a single `<phinterval>` element containing a minimal set of
#' non-overlapping, non-adjacent time spans.
#'
#' - `phint_squash()` takes a `<phinterval>` or `<Interval>` vector.
#' - `datetime_squash()` takes separate `start` and `end` datetime vectors.
#'
#' `phint_squash_by()` and `datetime_squash_by()` merge intervals within groups
#' defined by the `by` argument. The result is a `<phinterval>` vector containing
#' one element per unique value of `by`.
#'
#' @details
#'
#' These functions are particularly useful in aggregation workflows with
#' [dplyr::summarize()] to combine intervals within groups.
#'
#' The `phint_squash_by()` and `datetime_squash_by()` variants are designed to
#' replicate a call to [dplyr::group_by()] followed by [dplyr::summarize()], but
#' are typically faster. In particular, the following produce identical results:
#'
#' ```
#' phint_squash_by(phint, by = by)
#'
#' dplyr::tibble(phint = phint, by = by) |>
#'   dplyr::group_by(by) |>
#'   dplyr::summarize(phint = phint_squash(phint)) |>
#'   dplyr::ungroup()
#' ```
#'
#' @inheritParams params
#'
#' @param start `[POSIXct / POSIXlt / Date]`
#'
#' A vector of start times. Must be recyclable with `end`. Only used in
#' `datetime_squash()` and `datetime_squash_by()`.
#'
#' @param end `[POSIXct / POSIXlt / Date]`
#'
#' A vector of end times. Must be recyclable with `start`. Only used in
#' `datetime_squash()` and `datetime_squash_by()`.
#'
#' @param by `[vector / data.frame]`
#'
#' A grouping vector or data frame. Intervals are grouped by `by` and merged
#' separately within each group, returning one `<phinterval>` element per
#' unique value of `by`.
#'
#' For `datetime_squash_by()`, `by` must be recyclable with the recycled length
#' of `start` and `end`.
#'
#' `by` may be any vector in the vctrs sense. See [vctrs::obj_is_vector()]
#' for details.
#'
#' @param na_rm `[TRUE / FALSE]`
#'
#' Should `NA` elements be removed before squashing? If `FALSE` and any `NA`
#' elements are present, the result is `NA`. Defaults to `TRUE`.
#'
#' @param empty_to `["hole" / "na"]`
#'
#' How to handle empty inputs (length-0 vectors):
#' - `"hole"` (default): Return a hole.
#' - `"na"`: Return an `NA` phinterval.
#'
#' @param order_by `[TRUE / FALSE]`
#'
#' Should the output be ordered by the values in `by`? If `TRUE` (the default),
#' the output is sorted by the unique values of `by`. If `FALSE`, the output order
#' matches the first appearance of each group in `by`. Only used in `phint_squash_by()`
#' and `datetime_squash_by()`.
#'
#' @return
#'
#' `phint_squash()` and `datetime_squash()` return a length-1 `<phinterval>`
#' vector.
#'
#' `phint_squash_by()` and `datetime_squash_by()` return a [tibble::tibble()]
#' with columns `by` and `phint`, with one row per unique value of `by`.
#'
#' @seealso
#' [phint_flatten()] and [datetime_flatten()] to merge a `<phinterval>`
#' vector into a vector of scalar spans rather than a single element.
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
#' # Set na_rm = FALSE to propagate NA values
#' phint_squash(c(jan_1_to_5, jan_3_to_9, jan_11_to_12, NA), na_rm = FALSE)
#'
#' # empty_to determines the result of empty inputs
#' phint_squash(phinterval(), empty_to = "hole")
#' phint_squash(phinterval(), empty_to = "na")
#'
#' # phint_squash_by: squash within groups, returning a tibble
#' phint_squash_by(
#'   c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
#'   by = c("A", "A", "B")
#' )
#'
#' # datetime_squash_by: squash from start/end vectors within groups
#' datetime_squash_by(
#'   start = as.Date(c("2000-01-01", "2000-01-03", "2000-01-11")),
#'   end = as.Date(c("2000-01-05", "2000-01-09", "2000-01-12")),
#'   by = c("A", "A", "B")
#' )
#'
#' # Control output order with order_by
#' phint_squash_by(
#'   c(jan_1_to_5, jan_3_to_9, jan_11_to_12),
#'   by = c(2, 2, 1),
#'   order_by = TRUE
#' )
#'
#' @name squash
NULL

#' @rdname squash
#' @export
phint_squash <- function(phint, na_rm = TRUE, empty_to = c("hole", "na")) {
  check_bool(na_rm)
  out <- phint_unary_dispatch(
    x = phint,
    x_type = validate_type_phintish(phint),
    funs_cpp = list(
      phint = phint_squash_cpp,
      intvl = intvl_squash_cpp
    ),
    na_rm = na_rm,
    empty_to = arg_match0(empty_to, c("hole", "na"))
  )
  new_phinterval_bare(out, tzone = get_tzone(phint))
}


#' @rdname squash
#' @export
datetime_squash <- function(start, end, na_rm = TRUE, empty_to = c("hole", "na")) {
  check_datetime(start)
  check_datetime(end)
  check_recycleable(start, end)
  check_bool(na_rm)
  empty_to <- arg_match0(empty_to, c("hole", "na"))

  if (length(start) != length(end)) {
    range <- vec_recycle_common(start = start, end = end)
    start <- as.POSIXct(range$start)
    end <- as.POSIXct(range$end)
  } else {
    start <- as.POSIXct(start)
    end <- as.POSIXct(end)
  }

  out <- range_squash_cpp(
    starts = start,
    ends = end,
    na_rm = na_rm,
    empty_to = empty_to
  )
  new_phinterval_bare(out, tzone = tz_union(start, end))
}

#' @rdname squash
#' @export
phint_squash_by <- function(
    phint,
    by,
    na_rm = TRUE,
    empty_to = c("hole", "na"),
    order_by = TRUE
) {
  phint_type <- validate_type_phintish(phint)
  check_vector(by)
  check_recycleable_to(by, phint)
  check_bool(na_rm)
  empty_to <- arg_match0(empty_to, c("hole", "na"))
  check_bool(order_by)

  by_size <- vec_size(by)
  if (by_size == 0L) {
    # If we're here then `vec_size(phint)` is also 0
    out_phint <- switch(
      empty_to,
      hole = hole(tzone = get_tzone(phint)),
      na = na_phinterval(tzone = get_tzone(phint))
    )
    return(tibble::new_tibble(list(by = vec_init(by, 1L), phint = out_phint)))
  }

  if (by_size == 1L) {
    out <- phint_unary_dispatch(
      x = phint,
      x_type = phint_type,
      funs_cpp = list(
        phint = phint_squash_cpp,
        intvl = intvl_squash_cpp
      ),
      na_rm = na_rm,
      empty_to = empty_to
    )
    return(tibble::new_tibble(list(
      by = by,
      phint = new_phinterval_bare(out, tzone = get_tzone(phint))
    )))
  }

  groups <- if (order_by) vec_locate_sorted_groups(by) else vec_group_loc(by)
  out <- phint_unary_dispatch(
    x = phint,
    x_type = phint_type,
    funs_cpp = list(
      phint = phint_squash_by_cpp,
      intvl = intvl_squash_by_cpp
    ),
    na_rm = na_rm,
    empty_to = empty_to,
    group_locs = .subset2(groups, "loc")
  )

  tibble::new_tibble(list(
    by = .subset2(groups, "key"),
    phint = new_phinterval_bare(out, tzone = get_tzone(phint))
  ))
}

#' @rdname squash
#' @export
datetime_squash_by <- function(
    start,
    end,
    by,
    na_rm = TRUE,
    empty_to = c("hole", "na"),
    order_by = TRUE
) {
  check_datetime(start)
  check_datetime(end)
  check_recycleable(start, end)
  check_vector(by)
  check_bool(na_rm)
  empty_to <- arg_match0(empty_to, c("hole", "na"))
  check_bool(order_by)

  if (length(start) != length(end)) {
    range <- vec_recycle_common(start = start, end = end)
    start <- as.POSIXct(range$start)
    end <- as.POSIXct(range$end)
  } else {
    start <- as.POSIXct(start)
    end <- as.POSIXct(end)
  }
  check_recycleable_to(
    x = by,
    to = start,
    to_arg = "vctrs::vec_recycle_common(start, end)"
  )

  by_size <- vec_size(by)
  if (by_size == 0L) {
    # If we're here then `vec_size(start), vec_size(end)` are also 0
    out_phint <- switch(
      empty_to,
      hole = hole(tzone = tz_union(start, end)),
      na = na_phinterval(tzone = tz_union(start, end))
    )
    return(tibble::new_tibble(list(by = vec_init(by, 1L), phint = out_phint)))
  }

  if (by_size == 1L) {
    out <- range_squash_cpp(
      starts = start,
      ends = end,
      na_rm = na_rm,
      empty_to = empty_to
    )
    return(tibble::new_tibble(list(
      by = by,
      phint = new_phinterval_bare(out, tzone = tz_union(start, end))
    )))
  }

  groups <- if (order_by) vec_locate_sorted_groups(by) else vec_group_loc(by)
  out <- range_squash_by_cpp(
    starts = start,
    ends = end,
    group_locs = .subset2(groups, "loc"),
    na_rm = na_rm,
    empty_to = empty_to
  )

  tibble::new_tibble(list(
    by = .subset2(groups, "key"),
    phint = new_phinterval_bare(out, tzone = tz_union(start, end))
  ))
}
