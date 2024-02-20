# TODO Ethan:
# - document these
# - go through these functions call-by-call and look at `positions`, `is_start`
#   to see how instants (ex. [0,0]) and aligned edges (ex. [-1, 0], [0, 1])
#   are treated / interact
#     - I think it's fine to choose any strategy for treating these edge cases,
#       as long as it's well defined in the documentation

# TODO:
# I think that here an elsewhere, it might be easier to remove the instants from
# the ranges BEFORE doing anything (where required) and THEN deal with them separately.
#
# You could write custom functions for checking if instants intersect with one another
# (literally just using `%in%`) AND if instants appear in other ranges.

locate_overlaps <- function(phint, na.rm = TRUE, alignment = FALSE) {

  phint <- check_is_phinty(phint)
  tzone <- attr(phint, "tzone")

  ranges <- phint_to_ranges(phint, na.rm = na.rm)
  starts <- ranges$starts
  ends   <- ranges$ends

  if (any(is.na(starts)) || rlang::is_empty(starts)) {
    return(list(n = NA_integer_, spans = NA_phinterval(tzone = tzone)))
  }

  overlaps <- locate_range_overlaps(starts, ends, alignment = alignment)
  n <- overlaps$n

  if (rlang::is_empty(n)) {
    list(n = NA_integer_, spans = NA_phinterval(tzone = tzone))
  } else {
    list(
      n = n,
      spans = new_phinterval(
        reference_time = lubridate::POSIXct(length(n), tz = tzone),
        range_starts = overlaps$starts,
        range_ends = overlaps$ends,
        tzone = tzone
      )
    )
  }

}

extract_overlaps <- function(
    phint,
    n,
    at = c("least", "most", "exactly"),
    na.rm = TRUE,
    alignment = FALSE
  ) {

  n <- as.integer(n)
  if (!isTRUE(n > 0L)) {
    cli::cli_abort("{.arg n} must be a scalar integer greater than 0.")
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

  overlaps <- locate_range_overlaps(
    starts = starts,
    ends = ends,
    n = n,
    at = at,
    alignment = alignment
  )

  n_overlap_regions <- length(overlaps$n)
  if (n_overlap_regions == 0) {
    return(NA_phinterval(tzone = tzone))
  }

  # `locate_range_overlaps()` returns lists `$starts` and `$ends` of ranges in
  # which exactly `n` overlaps occur. If `at %in% c("most", "least")` we must
  # flatten those ranges to get regions where `>= n` or `<= n` overlaps occur.
  if (n_overlap_regions > 1) {
    overlaps <- range_flatten(list_c(overlaps$starts), list_c(overlaps$ends))
    range_starts <- list(overlaps$starts)
    range_ends   <- list(overlaps$ends)
  } else {
    range_starts <- overlaps$starts
    range_ends   <- overlaps$ends
  }

  new_phinterval(
    reference_time = lubridate::POSIXct(1L, tz = tzone),
    range_starts = range_starts,
    range_ends = range_ends,
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

locate_range_overlaps <- function(
    starts,
    ends,
    n = NULL,
    alignment = FALSE,
    at = c("least", "most", "exactly")
  ) {

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = length(starts))

  position_order <- order(positions)
  positions  <- positions[position_order]
  is_start   <- is_start[position_order]
  starts_minus_ends <- cumsum((is_start - 1L) + is_start)

  n_overlaps <- unique(starts_minus_ends[starts_minus_ends > 0])
  if (!is.null(n)) {
    n_overlaps <- switch(
      at,
      "exactly" = n_overlaps[n_overlaps == n],
      "least" = n_overlaps[n_overlaps >= n],
      "most" = n_overlaps[n_overlaps <= n]
    )
  }

  if (rlang::is_empty(n_overlaps)) {
    return(list(n = integer(), starts = list(), ends = list()))
  }

  consecutive_same_pos  <- positions[-1L] == positions[-length(positions)]
  consecutive_same_side <- is_start[-1L] == is_start[-length(is_start)]
  aligned_side   <- c(consecutive_same_pos & consecutive_same_side, FALSE)
  aligned_range  <- c(consecutive_same_pos & !consecutive_same_side, FALSE)
  is_instant <- rep(starts == ends, 2)[position_order]

  # TODO Ethan: Really look into how this plays out with instants and aligned
  #             ranges (edges) so we can be precise about the rules here.
  n_overlaps_at <- map(
    n_overlaps,
    \(n) {
      which(
        starts_minus_ends == n &
          # Prevents aligned starts or ends from being counted as an overlap.
          # Ex. ([1, 2], [1, 3]) -> [1, 2], not -> ([1, 1], [1, 2]).
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
