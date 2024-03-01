# TODO:
# I think that here an elsewhere, it might be easier to remove the instants from
# the ranges BEFORE doing anything (where required) and THEN deal with them separately.
#
# You could write custom functions for checking if instants intersect with one another
# (literally just using `%in%`) AND if instants appear in other ranges.

# TODO: All of the names in this file are a little misleading, because we're really dealing
#       with sets of ranges. These functions are internal, so I don't think it
#       matters too much...
range_flatten <- function(starts, ends) {

  starts_length <- length(starts)
  if (starts_length == 0) {
    return(list(starts = double(), ends = double()))
  }
  if (starts_length == 1) {
    return(list(starts = starts, ends = ends))
  }

  positions <- c(starts, ends)
  is_start <- rep(c(TRUE, FALSE), each = starts_length)

  # Order with `!is_start` to flatten adjacent ranges (as well as overlapping)
  position_order <- order(positions, !is_start)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)
  flat_ends <- which(starts_minus_ends == 0L)

  list(
    starts = positions[c(1, flat_ends[-length(flat_ends)] + 1)],
    ends = positions[flat_ends]
  )
}

range_union <- function(x_starts, x_ends, y_starts, y_ends) {
  range_flatten(
    starts = c(x_starts, y_starts),
    ends = c(x_ends, y_ends)
  )
}

range_contains_overlaps <- function(starts, ends) {
  start_order <- order(starts)
  any(starts[start_order][-1L] < cummax(ends[start_order][-length(ends)]))
}

range_is_flat <- function(starts, ends) {
  start_order <- order(starts)
  all(starts[start_order][-1L] > cummax(ends[start_order][-length(ends)]))
}

# TODO Ethan: These describe the conditions for a range to be used in:
# - range_intersect
# - range_compliment
# - range_setdifference
# This kind of test should appear *somewhere* AND *once* in any `phinterval`
# creation process (except maybe in simple constructor `new_phinterval`).
stop_unsanitized_range <- function(starts, ends) {
  stopifnot(length(starts) == length(ends))
  stopifnot(all(!is.na(starts) & !is.na(ends)))
  stopifnot(all(starts <= ends))
  stopifnot(range_is_flat(starts, ends))
}

# non-overlapping and non-adjacent ranges --------------------------------------

# Everything in this section requires that input range sets contain only
# non-overlapping, non-adjacent, distinct ranges. This is the case for the range
# sets used internally by `phinterval`.

range_intersect <- function(
    x_starts,
    x_ends,
    y_starts,
    y_ends,
    instants = FALSE
  ) {

  positions <- c(x_starts, y_starts, x_ends, y_ends)
  is_start <- rep(c(TRUE, FALSE), each = length(x_starts) + length(y_starts))

  position_order <- order(positions, !is_start)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)
  intersection_starts <- which(abs(starts_minus_ends) == 2L)

  starts <- positions[intersection_starts]
  ends <- positions[intersection_starts + 1L]

  if (!instants) {
    non_instants <- ends - starts >= 1L
    starts <- starts[non_instants]
    ends <- ends[non_instants]
  }

  list(starts = starts, ends = ends)

}

range_intersects <- function(
    x_starts,
    x_ends,
    y_starts,
    y_ends,
    instants = FALSE
) {

  positions <- c(x_starts, y_starts, x_ends, y_ends)
  is_start <- rep(c(TRUE, FALSE), each = length(x_starts) + length(y_starts))

  position_order <- order(positions, !is_start)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)

  if (instants) {
    any(abs(starts_minus_ends) == 2L)
  } else {
    intersection_starts <- which(abs(starts_minus_ends) == 2L)
    starts <- positions[intersection_starts]
    ends <- positions[intersection_starts + 1L]
    any(ends - starts >= 1L)
  }

}

range_setdifference <- function(
    x_starts,
    x_ends,
    y_starts,
    y_ends,
    instants = FALSE
  ) {

  x_min_start <- min(x_starts)
  x_max_end <- max(x_ends)

  y_starts_order <- order(y_starts)
  y_starts <- y_starts[y_starts_order]
  y_ends <- y_ends[y_starts_order]

  y_compliment_starts <- y_ends[y_starts_order][-length(y_ends)]
  y_compliment_ends <- y_starts[y_starts_order][-1L]

  # Makes the "universe" of the compliment [x_min_start, x_max_end]. Otherwise,
  # setdiff excludes segments of x outside of [min(y_starts), max(y_ends)].
  if (y_starts[[1]] > x_min_start) {
    y_compliment_starts <- c(x_min_start, y_compliment_starts)
    y_compliment_ends <- c(y_starts[[1]], y_compliment_ends)
  }
  if (y_ends[[length(y_ends)]] < x_max_end) {
    y_compliment_starts <- c(y_ends[[length(y_ends)]], y_compliment_starts)
    y_compliment_ends <- c(x_max_end, y_compliment_ends)
  }

  range_intersect(
    x_starts = x_starts,
    x_ends = x_ends,
    y_starts = y_compliment_starts,
    y_ends = y_compliment_ends,
    instants = instants
  )
}

range_within <- function(x_starts, x_ends, y_starts, y_ends) {

  y_order <- order(y_starts)
  identical(
    range_flatten(c(x_starts, y_starts), c(x_ends, y_ends)),
    list(starts = y_starts[y_order], ends = y_ends[y_order])
  )

}
