# TODO: Document
#' @export
phint_squash <- function(x, by = NULL, na.rm = TRUE) {
  # TODO: Input validation

  # TODO: This would be the output of a `check_type_phintish()`
  type <- if (is_phinterval(x)) "phinterval" else "Interval"

  tzone <- get_tzone(x)
  if (is_empty(x)) {
    return(new_empty_phinterval(tzone = tzone))
  }
  if (!na.rm && anyNA(x)) {
    return(new_na_phinterval(tzone = tzone))
  }

  if (!is.null(by)) {
    groups <- vec_group_loc(by)
    out <- switch(
      type,
      phinterval = phint_squash_by_cpp(
        size = field(x, "size"),
        starts = field(x, "starts"),
        ends = field(x, "ends"),
        group_locs = groups[["loc"]],
        na_rm = na.rm
      ),
      Interval = intvl_squash_by_cpp(
        starts = lubridate::int_start(x),
        spans = lubridate::int_length(x),
        group_locs = groups[["loc"]],
        na_rm = na.rm
      )
    )
    return(new_phinterval_bare(out, tzone = tzone))
  }

  if (type == "Interval") {
    out <- intvl_squash_cpp(
      starts = lubridate::int_start(x),
      spans = lubridate::int_length(x),
      na_rm = na.rm
    )
    return(new_phinterval_bare(out, tzone = tzone))
  }

  # `unlist()` removes `NULL` elements (NAs) from `starts` and `ends`
  all_starts <- unlist(field(x, "starts"), use.names = FALSE)
  all_ends <- unlist(field(x, "ends"), use.names = FALSE)

  # If all elements were NA and `na.rm = TRUE`, return NA
  if (is_empty(all_starts)) {
    return(new_na_phinterval(tzone = tzone))
  }

  # `phint_squash_cpp()` assumes endpoints are non-empty and non-NA
  new_phinterval_bare(
    fields = phint_squash_cpp(starts = all_starts, ends = all_ends),
    tzone = tzone
  )
}

# TODO: Document
#' @export
datetime_squash <- function(start, end, by = NULL, na.rm = FALSE) {
  # TODO: Input validation
  stopifnot(is.POSIXct(start), is.POSIXct(end))

  tzone <- tz_union(start, end)
  if (is_empty(x)) {
    return(new_empty_phinterval(tzone = tzone))
  }
  if (!na.rm && anyNA(x)) {
    return(new_na_phinterval(tzone = tzone))
  }

  if (!is.null(by)) {
    groups <- vec_group_loc(by)
    out <- datetime_squash_by_cpp(
      starts = start,
      ends = end,
      group_locs = groups[["loc"]],
      na_rm = na.rm
    )
  } else {
    out <- datetime_squash_cpp(
      starts = start,
      ends = end,
      na_rm = na.rm
    )
  }
  new_phinterval_bare(out, tzone = tzone)
}
