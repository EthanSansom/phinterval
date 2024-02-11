# TODO Ethan:
# I think this should be named to `count_overlaps` and it's partner function to
# get the overlaps should be `extract_overlaps`.

# TODO Ethan:
# - add documentation
# - add a function like `extract_overlaps` which returns a `phinterval` containing
#   the time when there where ANY overlaps (i.e. n > 2). This is helpful for when
#   you just want to know when overlaps exist (ex. when are you working multiple jobs)

# NOTE:
# This counts the number of intersections at any given time for
# a phinterval vector. The output looks like this:
#
# n: 1, 2, 4
# span: <phint1>, <phint2>, <phint4>
#
# where `<phint1>` are the spans where there was exactly 1 intersection, <phint2>
# when there was exactly 2 intersections, etc.
#
# There is no option to count AT LEAST n intersections, but that is easy to get
# from the result. Just accumulate the union of spans for n, n + 1, n + 2, ... to
# get the locations of at least `n` overlaps.
#
### Include in the example:
## Get the `phintervals` which contain at least `n` overlapping spans
# overlaps <- count_overlapping_spans(phint)
# overlaps$span <- accumulate(overlaps$span, union)
# overlaps

count_overlapping_spans <- function(phint, na_ignore = TRUE) {

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- attr(phint, "tzone")

  starts <- as.double(list_c(map2(reference_time, range_starts, `+`)))
  ends   <- as.double(list_c(map2(reference_time, range_ends, `+`)))

  na_positions <- is.na(starts) | is.na(ends)
  if (all(na_positions)) {
    return(list(n = NA_integer_, spans = NA_phinterval(tzone = tzone)))
  }
  if (na_ignore) {
    starts <- starts[!na_positions]
    ends <- ends[!na_positions]
  } else if (any(na_positions)) {
    return(list(n = NA_integer_, spans = NA_phinterval(tzone = tzone)))
  }

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = length(starts))

  position_order <- order(positions)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)

  # In this set-up, equal and adjacent ends (or starts) are counted as a range.
  # Ex. ([1, 2], [1, 3]) will produce ranges [1, 1], [1, 2], [2, 3]. This removes
  # the [1, 1] range - i.e. those produced from equal and adjacent starts (ends).
  consequitive_same_pos  <- positions[-1L] == positions[-length(positions)]
  consequitive_same_side <- is_start[-1L] == is_start[-length(is_start)]
  adjacent_same_side <- c(consequitive_same_pos & consequitive_same_side, FALSE)

  n_spans <- unique(starts_minus_ends[starts_minus_ends > 0])
  n_spans_at <- map(n_spans, \(n) which(starts_minus_ends == n & !adjacent_same_side))

  is_empty_span <- map_lgl(n_spans_at, rlang::is_empty)
  n_spans    <- n_spans[!is_empty_span]
  n_spans_at <- n_spans_at[!is_empty_span]

  # Setting the reference time as the unix epoch, as `starts` and `ends` are the
  # number of seconds from this time
  n_spans_starts <- map(n_spans_at, \(at) positions[at])
  n_spans_ends   <- map(n_spans_at, \(at) positions[at + 1L])
  reference_time <- .POSIXct(rep.int(0, length(n_spans)), tz = tzone)

  # There is one edge case to make a discontinuous `phinterval`.
  # Ex. If you have the spans [1, 3], [2, 2], then the output spans for n = 1 are
  # [1, 2], [2, 3], which must then be combined. I'm using `range_flatten` here,
  # but you could probably make something a little more optimized.
  n_spans_range <- map2(n_spans_starts, n_spans_ends, range_flatten)
  n_spans_starts <- map(n_spans_range, `[[`, "starts")
  n_spans_ends   <- map(n_spans_range, `[[`, "ends")

  list(
    n = n_spans,
    spans = new_phinterval(
      reference_time = reference_time,
      range_starts = n_spans_starts,
      range_ends = n_spans_ends,
      tzone = tzone
    )
  )

}

phint_to_ranges <- function(phint, na_ignore = TRUE) {

  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")

  starts <- as.double(list_c(map2(reference_time, range_starts, `+`)))
  ends   <- as.double(list_c(map2(reference_time, range_ends, `+`)))

  na_positions <- is.na(starts) | is.na(ends)
  if (na_ignore) {
    starts <- starts[!na_positions]
    ends <- ends[!na_positions]
  }

  list(starts = starts, ends = ends)

}

# TODO Ethan:
# - add back the include = c("minimum", "exact") option. Once you've split `count_overlapping_spans`
#   into more reasonable functions - it'll be easier to use parts of it for each option.
#   Note that `exact` is basically the same as `count_overlapping_spans`, you just want
#   only `n = n_overlaps`.

extract_overlaps <- function(phint, n_overlaps, na_ignore = TRUE) {

  # TODO Ethan: ALL of the code at the front is the same, extract this to a function
  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- attr(phint, "tzone")

  starts <- as.double(list_c(map2(reference_time, range_starts, `+`)))
  ends   <- as.double(list_c(map2(reference_time, range_ends, `+`)))

  na_positions <- is.na(starts) | is.na(ends)
  if (all(na_positions)) {
    return(list(n = NA_integer_, spans = NA_phinterval(tzone = tzone)))
  }
  if (na_ignore) {
    starts <- starts[!na_positions]
    ends <- ends[!na_positions]
  } else if (any(na_positions)) {
    return(list(n = NA_integer_, spans = NA_phinterval(tzone = tzone)))
  }

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = length(starts))

  position_order <- order(positions)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)


  comparison <- if (include == "count") `==` else `>=`
  overlapping_spans <- which(comparison(starts_minus_ends, n) & !adjacent_same_side)



}

