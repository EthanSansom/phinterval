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

range_contains_overlaps <- function(starts, ends) {
  start_order <- order(starts)
  any(starts[start_order][-1L] < cummax(ends[start_order][-length(ends)]))
}

range_is_flat <- function(starts, ends) {
  start_order <- order(starts)
  all(starts[start_order][-1L] > cummax(ends[start_order][-length(ends)]))
}

# TODO Ethan: These describe the conditions for a range to be used in:
# - range_intersection
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

range_intersection <- function(
    x_starts,
    x_ends,
    y_starts,
    y_ends,
    rm_instants = FALSE
  ) {

  positions <- c(x_starts, y_starts, x_ends, y_ends)
  is_start <- rep(c(TRUE, FALSE), each = length(x_starts) + length(y_starts))

  position_order <- order(positions)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)
  intersection_starts <- which(starts_minus_ends == 2L)

  starts <- positions[intersection_starts]
  ends <- positions[intersection_starts + 1L]

  if (rm_instants) {
    non_instants <- starts != ends
    list(
      starts = starts[non_instants],
      ends = ends[non_instants]
    )
  } else {
    list(starts = starts, ends = ends)
  }

}

range_setdifference <- function(x_starts, x_ends, y_starts, y_ends) {
  y_range_compliment <- range_compliment(
    starts = y_starts,
    ends = y_ends,
    lower_bound = min(x_starts),
    upper_bound = max(x_ends)
  )

  range_intersection(
    x_starts = x_starts,
    x_ends = x_ends,
    y_starts = y_range_compliment$starts,
    y_ends = y_range_compliment$ends,
    rm_instants = TRUE
  )
}

# TODO: Fix this. We want the compliment to extend from (lower_bound, min(starts)) and
#       (max(ends), upper_bound) IF the bounds exceed the input range. Otherwise,
#       we want to truncate. Currently, we do none of that.
range_compliment <- function(starts, ends, lower_bound = -Inf, upper_bound = Inf) {
  starts_order <- order(starts)
  range_truncate(
    starts = ends[starts_order][-length(ends)],
    ends = starts[starts_order][-1L],
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
}

range_truncate <- function(starts, ends, lower_bound, upper_bound) {
  in_bounds <- starts < upper_bound & ends > lower_bound
  starts <- starts[in_bounds]
  ends <- ends[in_bounds]

  starts[starts < lower_bound] <- lower_bound
  ends[ends > upper_bound] <- upper_bound

  list(starts = starts, ends = ends)
}
