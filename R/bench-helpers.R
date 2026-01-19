# classes ----------------------------------------------------------------------

# - <interval_sets> emulates the <phinterval> backend
# - <interval_spans> emulates the <Interval> backend

is_interval_setish <- function(x) {
  inherits(x, "interval_sets") || inherits(x, "interval_spans")
}

## interval_sets ---------------------------------------------------------------

#' @export
new_interval_sets <- function(size, starts, ends) {
  stopifnot(is.integer(size))
  stopifnot(is.list(starts))
  stopifnot(is.list(ends))

  vctrs::new_rcrd(
    fields = list(
      size = size,
      starts = starts,
      ends = ends
    ),
    class = "interval_sets"
  )
}

#' @export
is.na.interval_sets <- function(x) {
  is.na(field(x, "size"))
}

#' @export
format.interval_sets <- function(x, ...) {
  size <- field(x, "size")

  out <- paste0("{", map2_chr(
    .x = field(x, "starts"),
    .y = field(x, "ends"),
    .f = \(s, e) paste(paste0("[", s, ", ", e, "]"), collapse = ", ")
  ), "}")

  out[size == 0]   <- "{}"
  out[is.na(size)] <- NA_character_
  out
}

#' @export
as_interval_sets <- function(x) {
  UseMethod("as_interval_sets")
}

#' @export
as_interval_sets.interval_spans <- function(x) {
  na_at <- is.na(x)

  size <- rep(1L, length(x))
  starts <- as.list(field(x, "starts"))
  ends <- as.list(field(x, "ends"))

  size[na_at] <- NA_integer_
  starts[na_at] <- list(NULL)
  ends[na_at] <- list(NULL)

  new_interval_sets(
    size = size,
    starts = starts,
    ends = ends
  )
}

## interval_spans --------------------------------------------------------------

#' @export
new_interval_spans <- function(starts, ends) {
  stopifnot(is.numeric(starts))
  stopifnot(is.numeric(ends))

  vctrs::new_rcrd(
    fields = list(
      starts = starts,
      ends = ends
    ),
    class = "interval_spans"
  )
}

#' @export
is.na.interval_spans <- function(x) {
  is.na(field(x, "starts")) | is.na(field(x, "ends"))
}

#' @export
format.interval_spans <- function(x, ...) {
  out <- paste0("{[", field(x, "starts"), ", ", field(x, "ends"), "]}")
  out[is.na(x)] <- NA_character_
  out
}

# functions --------------------------------------------------------------------

#' @export
interval_sets_intersect <- function(x, y) {
  stopifnot(is_interval_setish(x), is_interval_setish(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  out <- switch(
    paste(class(x)[[1]], class(y)[[1]], sep = "_"),
    interval_sets_interval_sets = phint_phint_intersect_cpp(
      x_size = field(x, "size"), x_starts = field(x, "starts"), x_ends = field(x, "ends"),
      y_size = field(y, "size"), y_starts = field(y, "starts"), y_ends = field(y, "ends")
    ),
    interval_sets_interval_spans = phint_intvl_intersect_cpp(
      x_starts = field(x, "starts"), x_ends = field(x, "ends"),
      y_size = field(y, "size"), y_starts = field(y, "starts"), y_ends = field(y, "ends")
    ),
    interval_spans_interval_sets = intvl_phint_intersect_cpp(
      x_size = field(x, "size"), x_starts = field(x, "starts"), x_ends = field(x, "ends"),
      y_starts = field(y, "starts"), y_ends = field(y, "ends")
    ),
    interval_spans_interval_spans = intvl_intvl_intersect_cpp(
      x_starts = field(x, "starts"), x_ends = field(x, "ends"),
      y_starts = field(y, "starts"), y_ends = field(y, "ends")
    ),
    stop(paste0("Operations unimplemented for types <", class(x)[[1]], "> and <", class(y)[[1]], ">."))
  )
  vctrs::new_rcrd(fields = out, class = "interval_sets")
}

#' @export
interval_sets_squash <- function(x, by = NULL, na.rm = TRUE) {
  stopifnot(is_interval_setish(x))
  if (inherits(x, "interval_spans")) x <- as_interval_sets(x)

  # Empty case
  if (length(x) == 0) {
    return(new_interval_sets(integer(), list(), list()))
  }

  # NA case
  if (!na.rm && anyNA(field(x, "size"))) {
    return(new_interval_sets(NA_integer_, list(NA_real_), list(NA_real_)))
  }

  # `by` case
  if (!is.null(by)) {
    stopifnot(obj_is_vector(by), length(by) == 1L || length(by) == length(x))

    groups <- vec_group_loc(by)
    out <- phint_squash_by_cpp(
      size = field(x, "size"),
      starts = field(x, "starts"),
      ends = field(x, "ends"),
      group_locs = groups[["loc"]],
      na_rm = na.rm
    )
    return(vctrs::new_rcrd(fields = out,class = "interval_sets"))
  }

  # Default case

  # `unlist()` removes `NULL` elements (NAs) from `starts` and `ends`
  all_starts <- unlist(field(x, "starts"), use.names = FALSE)
  all_ends <- unlist(field(x, "ends"), use.names = FALSE)

  # If all elements were `NA` and `na.rm = TRUE`, return an NA <phinterval>
  if (length(all_starts) == 0) {
    return(new_interval_sets(NA_integer_, list(NA_real_), list(NA_real_)))
  }

  vctrs::new_rcrd(
    fields = phint_squash_cpp(starts = all_starts, ends = all_ends),
    class = "interval_sets"
  )
}
