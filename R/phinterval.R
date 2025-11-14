# class ------------------------------------------------------------------------

methods::setOldClass(c("phinterval", "list", "vctrs_vctr"))

#' @export
phinterval <- function(intervals = list(), tzone = NULL) {
  if (!is_bare_list(intervals)) {
    check_is_phintish(intervals)
    intervals <- list(as_phinterval(intervals))
  } else {
    check_is_list_of_phintish(intervals)
    intervals <- map(intervals, as_phinterval)
  }
  check_valid_tzone(tzone, allow_null = TRUE)

  if (is_empty(intervals)) {
    return(new_phinterval(tzone = tzone %||% "UTC"))
  }
  new_phinterval(
    interval_sets = map(intervals, phint_to_interval_set, empty_to = "hole"),
    tzone = tzone %||% get_tzone(intervals[[1]])
  )
}

new_phinterval <- function(interval_sets = list(), tzone = "UTC") {
  new_vctr(
    if (!is.list(interval_sets)) list(interval_sets) else interval_sets,
    tzone = tzone,
    class = "phinterval"
  )
}

#' @export
obj_print_data.phinterval <- function(x, max_width = 90, ...) {
  check_number_whole(max_width, min = 1)
  if (length(x) == 0) {
    return(invisible(x))
  }

  # Truncating prior to formatting as format.phinterval is slow
  max_print <- getOption("max.print", 9999L)
  if (length(x) > max_print) {
    x_t <- x[seq_len(max_print)]
    out <- set_names(format(x_t, max_width = max_width), names(x_t))
    print(out, quote = FALSE)
    cat(" [ Omitted", length(x) - max_print, "entries ]\n")
    return(invisible(x))
  }

  out <- set_names(format(x, max_width = max_width), names(x))
  print(out, quote = FALSE)
  invisible(x)
}

#' @export
format.phinterval <- function(x, max_width = 90, ...) {
  check_number_whole(max_width, min = 1)

  out <- paste0("{",
    map2_chr(
      map(phint_starts(x), \(starts) format(starts, usetz = FALSE)),
      map(phint_ends(x), \(ends) format(ends, usetz = FALSE)),
      # <hole> elements are initially formatted as character(0L)
      \(starts, ends) paste(starts, ends, sep = "--", collapse = ", ") %0|% ""
    ),
  "}")

  n_spans <- n_spans(x)
  too_big <- nchar(out) > max_width
  out[too_big] <- paste0("<phint[", n_spans[too_big], "]>")
  out[!n_spans] <- "<hole>"
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype_abbr.phinterval <- function(x, ...) {
  tzone <- get_tzone(x)
  if (tz_is_local(tzone)) tzone <- "local" #nocov
  paste0("phint<", tzone, ">")
}

#' @export
vec_ptype_full.phinterval <- function(x, ...) {
  tzone <- get_tzone(x)
  if (tz_is_local(tzone)) tzone <- "local" #nocov
  paste0("phinterval<", tzone, ">")
}

#' @export
vec_proxy_equal.phinterval <- function(x, ...) {
  vec_data(x)
}

#' @export
vec_ptype2.phinterval.phinterval <- function(x, y, ...) {
  new_phinterval(tzone = tz_union(x, y))
}

#' @export
vec_ptype2.phinterval.Interval <- function(x, y, ...) {
  new_phinterval(tzone = tz_union(x, y))
}

#' @export
vec_ptype2.Interval.phinterval <- function(x, y, ...) {
  new_phinterval(tzone = tz_union(x, y))
}

#' @export
vec_cast.phinterval.phinterval <- function(x, to, ...) x

#' @export
vec_cast.phinterval.Interval <- function(x, to, ...) as_phinterval(x)

#' @export
is_phinterval <- function(x) {
  inherits(x, "phinterval")
}

#' @export
is_phintish <- function(x) {
  is_phinterval(x) || lubridate::is.interval(x)
}

#' @export
as_phinterval <- function(x, ...) {
  UseMethod("as_phinterval")
}

#' @export
as_phinterval.default <- function(x, ...) {
  vec_cast(x, new_phinterval())
}

#' @export
as_phinterval.Interval <- function(x, ...) {
  new_phinterval(
    interval_sets = cpp_lubridate_interval_to_interval_sets(
      starts = lubridate::int_start(x),
      spans = lubridate::int_length(x)
    ),
    tzone = get_tzone(x)
  )
}

# arithmetic -------------------------------------------------------------------

#' @export
#' @method vec_arith phinterval
vec_arith.phinterval <- function(op, x, y, ...) {
  UseMethod("vec_arith.phinterval", y)
}

#' @export
#' @method vec_arith.phinterval Duration
vec_arith.phinterval.Duration <- function(op, x, y, ...) {
  switch(
    op,
    "/" = as_duration(x) / y,
    "%/%" = trunc(as_duration(x) / y),
    stop_incompatible_op(op, x, y)
  )
}

# accessors --------------------------------------------------------------------

#' @export
n_spans <- function(phint) {
  UseMethod("n_spans")
}

#' @export
n_spans.default <- function(phint) {
  check_is_phintish(phint)
}

#' @export
n_spans.Interval <- function(phint) {
  out <- rep(1L, length(phint))
  out[is.na(phint)] <- NA_integer_
  out
}

#' @export
n_spans.phinterval <- function(phint) {
  out <- rep(NA_integer_, length(phint))
  non_na <- !is.na(phint)
  out[non_na] <- map_int(vec_data(phint)[non_na], nrow)
  out
}

#' @export
is_hole <- function(phint) {
  n_spans(phint) == 0L
}

#' @export
phint_start <- function(phint) {
  UseMethod("phint_start")
}

#' @export
phint_start.default <- function(phint) {
  check_is_phintish(phint)
}

#' @export
phint_start.Interval <- function(phint) {
  out <- lubridate::int_start(lubridate::int_standardize(phint))
  out[is.na(phint)] <- NA
  out
}

#' @export
phint_start.phinterval <- function(phint) {
  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(phint))
  cpp_interval_sets_start(vec_data(phint)) + origin
}

#' @export
phint_end <- function(phint) {
  UseMethod("phint_end")
}

#' @export
phint_end.default <- function(phint) {
  check_is_phintish(phint)
}

#' @export
phint_end.Interval <- function(phint) {
  out <- lubridate::int_end(lubridate::int_standardize(phint))
  out[is.na(phint)] <- NA
  out
}

#' @export
phint_end.phinterval <- function(phint) {
  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(phint))
  cpp_interval_sets_end(vec_data(phint)) + origin
}

#' @export
phint_starts <- function(phint) {
  UseMethod("phint_starts")
}

#' @export
phint_starts.default <- function(phint) {
  check_is_phintish(phint)
}

#' @export
phint_starts.Interval <- function(phint) {
  as.list(phint_start(phint))
}

#' @export
phint_starts.phinterval <- function(phint) {
  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(phint))
  map(cpp_interval_sets_starts(vec_data(phint)), `+`, origin)
}

#' @export
phint_ends <- function(phint) {
  UseMethod("phint_ends")
}

#' @export
phint_ends.default <- function(phint) {
  check_is_phintish(phint)
}

#' @export
phint_ends.Interval <- function(phint) {
  as.list(phint_end(phint))
}

#' @export
phint_ends.phinterval <- function(phint) {
  origin <- lubridate::with_tz(lubridate::origin, tzone = get_tzone(phint))
  map(cpp_interval_sets_ends(vec_data(phint)), `+`, origin)
}

#' @export
phint_length <- function(phint) {
  UseMethod("phint_length")
}

#' @export
phint_length.default <- function(phint) {
  check_is_phintish(phint)
}

#' @export
phint_length.Interval <- function(phint) {
  lubridate::int_length(phint)
}

#' @export
phint_length.phinterval <- function(phint) {
  map_dbl(phint_lengths(phint), sum)
}

#' @export
phint_lengths <- function(phint) {
  UseMethod("phint_lengths")
}

#' @export
phint_lengths.default <- function(phint) {
  check_is_phintish(phint)
}

#' @export
phint_lengths.Interval <- function(phint) {
  as.list(lubridate::int_length(phint))
}

#' @export
phint_lengths.phinterval <- function(phint) {
  data <- vec_data(phint)
  out <- map2(
    cpp_interval_sets_ends(data),
    cpp_interval_sets_starts(data),
    `-`
  )
  out[is_hole(phint)] <- 0
  out
}

# miscellaneous ----------------------------------------------------------------

#' @export
phint_invert <- function(phint, hole_to = c("hole", "inf", "na")) {
  UseMethod("phint_invert")
}

#' @export
phint_invert.default <- function(phint, hole_to = c("hole", "inf", "na")) {
  check_is_phintish(phint)
}

#' @export
phint_invert.Interval <- function(phint, hole_to = c("hole", "inf", "na")) {
  arg_match(hole_to)
  interval_sets <- rep(list(the$empty_interval_set), length(phint))
  interval_sets[is.na(phint)] <- list(the$na_interval_set)
  new_phinterval(interval_sets, tzone = get_tzone(phint))
}

#' @export
phint_invert.phinterval <- function(phint, hole_to = c("hole", "inf", "na")) {
  hole_to <- arg_match(hole_to)
  interval_sets <- cpp_invert_interval_sets(vec_data(phint))
  switch(
    hole_to,
    inf = interval_sets[is_hole(phint)] <- list(the$inf_interval_set),
    na = interval_sets[is_hole(phint)] <- list(the$na_interval_set)
  )
  new_phinterval(interval_sets = interval_sets, tzone = get_tzone(phint))
}

#' @export
phint_to_spans <- function(phint, hole_to = c("empty", "na", "null")) {
  hole_to <- arg_match(hole_to)
  out <- map2(phint_starts(phint), phint_ends(phint), interval)
  switch(
    hole_to,
    null = out[is_hole(phint)] <- list(NULL),
    na = out[is_hole(phint)] <- list(interval(NA, NA, tzone = get_tzone(phint)))
  )
  out
}

#' @export
phint_sift <- function(phint) {
  check_is_phintish(phint)
  new_phinterval(
    cpp_interval_sets_remove_instants(vec_data(as_phinterval(phint))),
    tzone = get_tzone(phint)
  )
}

#' @export
as_duration <- function(x) {
  UseMethod("as_duration")
}

#' @export
as_duration.default <- function(x) {
  lubridate::as.duration(x)
}

#' @export
as_duration.phinterval <- function(x) {
  lubridate::as.duration(phint_length(x))
}
