range_overlaps <- function(starts, ends) {

  start_order <- order(starts)
  any(starts[start_order][-1L] < cummax(ends[start_order][-length(ends)]))

}

range_flatten <- function(starts, ends) {

  positions <- c(starts, ends)
  is_start  <- rep(c(TRUE, FALSE), each = length(starts))

  # Ordering with `!is_start` to flatten adjacent ranges (as well as overlapping)
  position_order <- order(positions, !is_start)
  positions <- positions[position_order]
  is_start  <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)
  flat_ends <- which(starts_minus_ends == 0L)

  list(
    starts = positions[c(1, flat_ends[-length(flat_ends)] + 1)],
    ends = positions[flat_ends]
  )

}

range_intersection <- function(x_starts, x_ends, y_starts, y_ends, rm_instants = FALSE) {

  positions  <- c(x_starts, y_starts, x_ends, y_ends)
  is_start   <- rep(c(TRUE, FALSE), each = length(x_starts) + length(y_starts))

  position_order <- order(positions)
  positions  <- positions[position_order]
  is_start   <- is_start[position_order]

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
    list(
      starts = starts,
      ends = ends
    )
  }

}

range_compliment <- function(starts, ends, lower_bound = -Inf, upper_bound = Inf) {

  starts_order <- order(starts)

  if (identical(lower_bound, -Inf) && identical(upper_bound, Inf)) {
    list(
      starts = ends[starts_order][-length(ends)],
      ends = starts[starts_order][-1L]
    )
  } else {
    range_truncate(
      starts = ends[starts_order][-length(ends)],
      ends = starts[starts_order][-1L],
      lower_bound = lower_bound,
      upper_bound = upper_bound
    )
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

range_truncate <- function(starts, ends, lower_bound, upper_bound) {

  in_bounds <- starts < upper_bound & ends > lower_bound
  starts <- starts[in_bounds]
  ends <- ends[in_bounds]

  starts[starts < lower_bound] <- lower_bound
  ends[ends > upper_bound] <- upper_bound

  list(starts = starts, ends = ends)

}
