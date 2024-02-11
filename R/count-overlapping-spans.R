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

count_overlapping_spans <- function(phint, na.rm = TRUE) {

  phint <- check_is_phinty(phint)
  tzone <- attr(phint, "tzone")

  ranges <- phint_to_ranges(phint, na.rm = na.rm)
  starts <- ranges$starts
  ends   <- ranges$ends

  if (any(is.na(starts)) || rlang::is_empty(starts)) {
    return(list(n = NA_integer_, spans = NA_phinterval(tzone = tzone)))
  }

  overlapping_ranges <- count_overlapping_ranges(starts, ends)
  n <- overlapping_ranges$n

  list(
    n = n,
    spans = new_phinterval(
      reference_time = .POSIXct(rep(0, length(n)), tz = tzone),
      range_starts = overlapping_ranges$starts,
      range_ends = overlapping_ranges$ends,
      tzone = tzone
    )
  )

}

extract_overlaps <- function(
    phint,
    n_overlapping,
    na.rm = TRUE,
    include = c("minimum", "exact")
  ) {

  n_overlapping <- as.integer(n_overlapping)
  if (!isTRUE(n_overlapping > 0L)) {
    cli::cli_abort("{.arg {n_overlapping}} must be a scalar integer greater than 0.")
  }

  include <- rlang::arg_match(include)
  phint <- check_is_phinty(phint)
  tzone <- attr(phint, "tzone")

  ranges <- phint_to_ranges(phint, na.rm = na.rm)
  starts <- ranges$starts
  ends   <- ranges$ends

  if (any(is.na(starts)) || rlang::is_empty(starts)) {
    return(NA_phinterval(tzone = tzone))
  }

  if (include == "exact") {
    overlaps <- count_overlapping_ranges(starts, ends, n = n_overlapping)
  } else {
    overlaps <- extract_minimum_range_overlaps(starts, ends, n = n_overlapping)
  }

  new_phinterval(
    reference_time = .POSIXct(0, tz = tzone),
    range_starts = overlaps$starts,
    range_ends = overlaps$ends,
    tzone = tzone
  )

}

# helpers ----------------------------------------------------------------------

phint_to_ranges <- function(phint, na.rm = TRUE) {

  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")

  starts <- as.double(list_c(map2(reference_time, range_starts, `+`)))
  ends   <- as.double(list_c(map2(reference_time, range_ends, `+`)))

  if (na.rm) {
    na_positions <- is.na(starts) | is.na(ends)
    starts <- starts[!na_positions]
    ends <- ends[!na_positions]
  }

  list(starts = starts, ends = ends)

}

count_overlapping_ranges <- function(starts, ends, n = NULL) {

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = length(starts))

  position_order <- order(positions)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)

  # In this set-up, equal and adjacent ends (or starts) are counted as a range.
  # ([1, 2], [1, 3]) produce ranges [1, 1], [1, 2], [2, 3]. We remove the [1, 1]
  # range which is produced by the two 1, 1 starts.
  consequitive_same_pos  <- positions[-1L] == positions[-length(positions)]
  consequitive_same_side <- is_start[-1L] == is_start[-length(is_start)]
  adjacent_same_side <- c(consequitive_same_pos & consequitive_same_side, FALSE)

  # If `n` is NULL, get ranges containing any number of overlaps. Otherwise, get
  # only the ranges containing exactly `n` overlaps.
  n_overlaps <- if (is.null(n)) unique(starts_minus_ends[starts_minus_ends > 0]) else n
  n_overlaps_at <- map(n_overlaps, \(n) which(starts_minus_ends == n & !adjacent_same_side))

  is_empty_range <- map_lgl(n_overlaps_at, rlang::is_empty)
  n_overlaps    <- n_overlaps[!is_empty_range]
  n_overlaps_at <- n_overlaps_at[!is_empty_range]

  n_overlaps_starts <- map(n_overlaps_at, \(at) positions[at])
  n_overlaps_ends   <- map(n_overlaps_at, \(at) positions[at + 1L])

  # There is one edge case to make a range with a point discontinuity.
  # Given input ranges [1, 3], [2, 2], the outputs for n = 1 are [1, 2], [2, 3],
  # which must then be combined in preparation for use in `new_phinterval`. Use
  # `range_flatten` to resolve for now, but could make something more optimized.
  n_overlaps_range <- map2(n_overlaps_starts, n_overlaps_ends, range_flatten)
  n_overlaps_starts <- map(n_overlaps_range, `[[`, "starts")
  n_overlaps_ends   <- map(n_overlaps_range, `[[`, "ends")

  return(
    list(
      n = n_overlaps,
      starts = n_overlaps_starts,
      ends = n_overlaps_ends
    )
  )

}

# TODO Ethan: This file's pretty messy, and a lot of these range operations can
# probably be moved to `range-operations`. Think about the naming schemes for
# range operations, since some of them operate in parallel (have a length preserving
# output) and others do not (like this one).
extract_minimum_range_overlaps <- function(starts, ends, n) {

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = length(starts))

  position_order <- order(positions)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)
  is_increasing <- c(
    TRUE, starts_minus_ends[-length(starts_minus_ends)] < starts_minus_ends[-1L]
  )
  overlap_starts <- positions[which(starts_minus_ends == n & is_increasing)]
  overlap_ends   <- positions[which(starts_minus_ends + 1L == n & !is_increasing)]

  list(starts = list(overlap_starts), ends = list(overlap_ends))

}
