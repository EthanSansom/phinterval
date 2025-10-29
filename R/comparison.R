# `phinterval`s are equal when they represent the same collection of time-spans,
# ignoring timezone. This is unlike `Interval` equality, which is determined by
# duration of the time-span. https://github.com/tidyverse/lubridate/issues/1135

#' @export
vec_proxy_equal.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")

  starts_uid <- rep(NA_character_, length(reference_time))
  ends_uid <- starts_uid

  # TODO: This feels illegal, but should (?) uniquely ID a set of time spans
  non_na_at <- !is.na(reference_time)
  seconds <- as.double(reference_time)[non_na_at]
  starts <- range_starts[non_na_at]
  ends <- range_ends[non_na_at]

  starts_uid[non_na_at] <- map2(seconds, starts, \(t, s) t + sort(s)) |> map_chr(range_to_uid)
  ends_uid[non_na_at] <- map2(seconds, ends, \(t, e) t + sort(e)) |> map_chr(range_to_uid)

  data.frame(
    range_starts = starts_uid,
    range_ends = ends_uid
  )
}

range_to_uid <- function(x) paste0(sort(x), collapse = ",")

#' @export
vec_proxy_compare.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")

  min_start <- map2_dbl(reference_time, range_starts, \(t, s) t + min(s))
  max_end <- map2_dbl(reference_time, range_ends, \(t, e) t + max(e))
  duration_seconds <- map2_dbl(range_starts, range_ends, \(s, e) sum(e - s))

  # `phinterval`s are ordered by start, end, and duration.
  data.frame(
    starts_first = min_start,
    ends_first = -max_end,
    shortest = -duration_seconds
  )
}
