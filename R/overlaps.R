# There is no option to count AT LEAST n intersections, but that is easy to get
# from the result. Just accumulate the union of spans for n, n + 1, n + 2, ... to
# get the locations of at least `n` overlaps.
#
### Include in the example:
## Get the `phintervals` which contain at least `n` overlapping spans
# overlaps <- count_overlapping_spans(phint)
# overlaps$span <- accumulate(overlaps$span, union)
# overlaps

# TODO Ethan: This might actually be better named, `locate_overlaps`, since it
# provides the location AND count

locate_overlaps <- function(phint, na.rm = TRUE, alignment = FALSE) {

  phint <- check_is_phinty(phint)
  tzone <- attr(phint, "tzone")

  ranges <- phint_to_ranges(phint, na.rm = na.rm)
  starts <- ranges$starts
  ends   <- ranges$ends

  if (any(is.na(starts)) || rlang::is_empty(starts)) {
    return(list(n = NA_integer_, spans = NA_phinterval(tzone = tzone)))
  }

  overlaps <- count_overlapping_ranges(starts, ends, alignment = alignment)
  n <- overlaps$n

  list(
    n = n,
    spans = new_phinterval(
      reference_time = .POSIXct(rep(0, length(n)), tz = tzone),
      range_starts = overlaps$starts,
      range_ends = overlaps$ends,
      tzone = tzone
    )
  )

}

# Wrapper for `locate_overlaps()$spans[locate_overlaps()$n == n]`
extract_exact_overlap <- function(phint, n, na.rm = TRUE, alignment = FALSE) {

}

# Extract locations where at least/most `n` overlaps occur
extract_multi_overlap <- function(phint, n, at = c("least", "most"), na.rm = TRUE, alignment = FALSE) {

}

# TODO Ethan:
# I think this whole alignment AND instants interacting issue would be easier if
# you just removed the instants in the first pass, THEN added them back afterwards.
# I.e. You would do another pass of JUST instants, first counting how many overlap
# with themselves (ex. [0, 0], [0, 0] is 1 overlap) and then counting how many
# of them overlap with the other ranges. That way, you could also specify that
# instants CANT overlap with the edges of other ranges.
#
# You know what, no. If you want instants to overlap, then you have to have
# alignment = TRUE, since instants that overlap ARE aligned with one another
# (ex. they have the same start).

# TODO Ethan:
# - there are faster ways to do the `extract_overlaps` operations, but I think
#   maybe just wrapping `locate_overlaps` and then merging as required for
#   the at least, at most situations would be easier. Plus, you only need one
#   good implementations to handle the `alignment` and instants problems.

extract_overlaps <- function(
    phint,
    n_overlapping,
    at = c("least", "most", "exactly"),
    na.rm = TRUE,
    alignment = FALSE
  ) {

  n_overlapping <- as.integer(n_overlapping)
  if (!isTRUE(n_overlapping > 0L)) {
    cli::cli_abort("{.arg {n_overlapping}} must be a scalar integer greater than 0.")
  }

  at <- rlang::arg_match(at)
  phint <- check_is_phinty(phint)
  tzone <- attr(phint, "tzone")

  ranges <- phint_to_ranges(phint, na.rm = na.rm)
  starts <- ranges$starts
  ends   <- ranges$ends

  if (any(is.na(starts)) || rlang::is_empty(starts)) {
    return(NA_phinterval(tzone = tzone))
  }

  # TODO Ethan: Implement an option for `at = "most"`
  overlaps <- switch(
    at,
    "exactly" = count_overlapping_ranges(starts, ends, n = n_overlapping, alignment = alignment),
    "least" = extract_minimum_range_overlaps(starts, ends, n = n_overlapping, alignment = alignment),
    "most" = stop("`at == 'most' not implemented")
  )

  # TODO Ethan:
  # Currently returning an NA phinterval when the number of overlaps doesn't exist.
  # Think about if this is the best approach, could return an empty phinterval also.
  range_starts <- overlaps$starts
  range_ends   <- overlaps$ends
  if (rlang::is_empty(range_starts)) {
    NA_phinterval(tzone = tzone)
  } else {
    new_phinterval(
      reference_time = .POSIXct(0, tz = tzone),
      range_starts = range_starts,
      range_ends = range_ends,
      tzone = tzone
    )
  }

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

range_to_positions <- function(starts, ends) {

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = length(starts))

  position_order <- order(positions)
  positions  <- positions[position_order]
  is_start   <- is_start[position_order]

  list(
    positions = positions,
    is_start = is_start
  )

}

# TODO Ethan:
# Document this function. `alignment = TRUE` means that you want to count the point
# of alignment as an overlap. Ex. [1, 2], [2, 3] overlap at [2, 2].
count_overlapping_ranges <- function(
    starts,
    ends,
    n = NULL,
    alignment = FALSE
  ) {

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = length(starts))

  position_order <- order(positions)
  positions  <- positions[position_order]
  is_start   <- is_start[position_order]
  is_instant <- rep(starts == ends, 2)[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)

  consecutive_same_pos  <- positions[-1L] == positions[-length(positions)]
  consecutive_same_side <- is_start[-1L] == is_start[-length(is_start)]
  aligned_side   <- c(consecutive_same_pos & consecutive_same_side, FALSE)
  aligned_range  <- c(consecutive_same_pos & !consecutive_same_side, FALSE)

  n_overlaps <- n %||% unique(starts_minus_ends[starts_minus_ends > 0])
  n_overlaps_at <- map(
    n_overlaps,
    \(n) {
      which(
        starts_minus_ends == n &
          # Prevents aligned starts or ends from being counted as an overlap.
          # Ex. ([1, 2], [1, 3]) -> [1, 2], NOT -> ([1, 1], [1, 2]).
          !aligned_side &
          # Prevents adjacent ranges from being counted as overlapping, unless
          # the range is instantaneous.
          (!aligned_range | is_instant | alignment)
      )
    }
  )

  is_empty_range <- map_lgl(n_overlaps_at, rlang::is_empty)
  n_overlaps    <- n_overlaps[!is_empty_range]
  n_overlaps_at <- n_overlaps_at[!is_empty_range]

  n_overlaps_starts <- map(n_overlaps_at, \(at) positions[at])
  n_overlaps_ends   <- map(n_overlaps_at, \(at) positions[at + 1L])

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
extract_minimum_range_overlaps <- function(starts, ends, n, alignment = FALSE) {

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = length(starts))

  position_order <- order(positions)
  positions  <- positions[position_order]
  is_start   <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)
  is_increasing <- c(
    TRUE, starts_minus_ends[-length(starts_minus_ends)] < starts_minus_ends[-1L]
  )

  overlap_starts <- positions[which(starts_minus_ends == n & is_increasing)]
  overlap_ends   <- positions[which(starts_minus_ends + 1L == n & !is_increasing)]

  list(starts = list(overlap_starts), ends = list(overlap_ends))

}
