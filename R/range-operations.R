# positive ranges --------------------------------------------------------------

# Functions in this section require only that input range sets contain only
# positive ranges (ex. [2, 5], [-1, 1], not [5, 2], [1, -1]), which may be
# adjacent or overlapping.

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

range_is_flat <- function(starts, ends) {
  start_order <- order(starts)
  all(starts[start_order][-1L] > cummax(ends[start_order][-length(ends)]))
}

range_union <- function(x_starts, x_ends, y_starts, y_ends) {
  range_flatten(
    starts = c(x_starts, y_starts),
    ends = c(x_ends, y_ends)
  )
}

# non-overlapping and non-adjacent ranges --------------------------------------

# All functions below require that input range sets contain only positive,
# non-overlapping, non-adjacent, ranges. This is the case for the range sets
# used internally by `phinterval`.

range_intersect <- function(
    x_starts,
    x_ends,
    y_starts,
    y_ends,
    inclusive = FALSE
  ) {

  if (!inclusive) {
    x_non_instants <- x_starts != x_ends
    y_non_instants <- y_starts != y_ends

    x_starts <- x_starts[x_non_instants]
    x_ends <- x_ends[x_non_instants]
    y_starts <- y_starts[y_non_instants]
    y_ends <- y_ends[y_non_instants]
  }

  positions <- c(x_starts, y_starts, x_ends, y_ends)
  is_start <- rep(c(TRUE, FALSE), each = length(x_starts) + length(y_starts))

  position_order <- order(positions, if (inclusive) !is_start else is_start)
  positions <- positions[position_order]
  is_start <- is_start[position_order]

  starts_minus_ends <- cumsum((is_start - 1L) + is_start)
  intersection_starts <- which(abs(starts_minus_ends) == 2L)

  starts <- positions[intersection_starts]
  ends <- positions[intersection_starts + 1L]

  list(starts = starts, ends = ends)
}

range_intersects <- function(
    x_starts,
    x_ends,
    y_starts,
    y_ends,
    inclusive = FALSE
  ) {

  if (!inclusive) {
    x_non_instants <- x_starts != x_ends
    y_non_instants <- y_starts != y_ends

    x_starts <- x_starts[x_non_instants]
    x_ends <- x_ends[x_non_instants]
    y_starts <- y_starts[y_non_instants]
    y_ends <- y_ends[y_non_instants]
  }

  starts <- c(x_starts, y_starts)
  ends <- c(x_ends, y_ends)
  start_order <- order(starts)

  if (inclusive) {
    any(starts[start_order][-1L] <= cummax(ends[start_order][-length(ends)]))
  } else {
    any(starts[start_order][-1L] < cummax(ends[start_order][-length(ends)]))
  }
}

range_setdifference <- function(
    x_starts,
    x_ends,
    y_starts,
    y_ends
  ) {

  if (rlang::is_empty(x_starts) || rlang::is_empty(y_starts)) {
    return(list(starts = x_starts, ends = x_ends))
  }

  is_x_instant <- x_starts == x_ends
  if (!any(is_x_instant)) {
    return(range_setdifference_happy_path(x_starts, x_ends, y_starts, y_ends))
  }

  # Remove any instants which are shared between x and y.
  is_y_instant <- y_starts == y_ends
  if (any(is_y_instant)) {
    y_instants <- y_starts[is_y_instant]
    is_in_y <- map_lgl(
      x_starts[is_x_instant],
      \(x_instant) x_instant %in% y_instants
    )

    non_shared_instants <- !(is_in_y & is_x_instant)
    x_starts <- x_starts[non_shared_instants]
    x_ends <- x_ends[non_shared_instants]

    is_x_instant <- x_starts == x_ends
    if (!any(is_x_instant)) {
      return(range_setdifference_happy_path(x_starts, x_ends, y_starts, y_ends))
    }

    # Now that shared instants are removed, if all ranges in y are instants than
    # we can return the remaining ranges in x.
    is_y_instant <- y_starts == y_ends
    if (all(is_y_instant)) {
      return(list(starts = x_starts, ends = x_ends))
    }

    # Don't want adjacent ranges (ex. [0, 1], [1, 2]) in the output, so removing
    # any y instants which could split an x range into two adjacent ranges.
    y_starts <- y_starts[!is_y_instant]
    y_ends <- y_ends[!is_y_instant]
  }

  y_starts_order <- order(y_starts)
  y_starts <- y_starts[y_starts_order]
  y_ends <- y_ends[y_starts_order]

  y_comp_starts <- y_ends[y_starts_order][-length(y_ends)]
  y_comp_ends <- y_starts[y_starts_order][-1L]

  # Makes the "universe" of the compliment [x_min_start, x_max_end]. Otherwise,
  # setdiff excludes segments of x outside of [min(y_starts), max(y_ends)].
  x_min_start <- min(x_starts)
  x_max_end <- max(x_ends)
  if (y_starts[[1]] > x_min_start) {
    y_comp_starts <- c(x_min_start, y_comp_starts)
    y_comp_ends <- c(y_starts[[1]], y_comp_ends)
  }
  if (y_ends[[length(y_ends)]] < x_max_end) {
    y_comp_starts <- c(y_ends[[length(y_ends)]], y_comp_starts)
    y_comp_ends <- c(x_max_end, y_comp_ends)
  }

  # Keep instants in x which are within the compliment (not on the edge).
  x_instants <- x_starts[is_x_instant]
  is_setdiff_instant <- y_comp_starts < x_instants & x_instants < y_comp_ends

  # If any instants are within the setdifference, have to add them manually.
  if (any(is_setdiff_instant)) {
    setdiff_instants <- x_instants[is_setdiff_instant]
    setdiff <- range_intersect(
      x_starts = x_starts,
      x_ends = x_ends,
      y_starts = y_comp_starts,
      y_ends = y_comp_ends,
      inclusive = FALSE
    )
    list(
      starts = c(setdiff_instants, setdiff$starts),
      ends = c(setdiff_instants, setdiff$ends)
    )
  } else {
    range_intersect(
      x_starts = x_starts,
      x_ends = x_ends,
      y_starts = y_comp_starts,
      y_ends = y_comp_ends,
      inclusive = FALSE
    )
  }
}

# Set difference in the simple case where x contains no instants
range_setdifference_happy_path <- function(x_starts, x_ends, y_starts, y_ends) {
  # Don't want instantaneous splits in the output ranges (ex. [0, 1], [1, 2]),
  # so removing any y instants which could cause this split.
  is_y_instant <- y_starts == y_ends
  if (all(is_y_instant)) {
    return(list(starts = x_starts, ends = x_ends))
  }
  y_starts <- y_starts[!is_y_instant]
  y_ends <- y_ends[!is_y_instant]

  y_starts_order <- order(y_starts)
  y_starts <- y_starts[y_starts_order]
  y_ends <- y_ends[y_starts_order]

  y_comp_starts <- y_ends[y_starts_order][-length(y_ends)]
  y_comp_ends <- y_starts[y_starts_order][-1L]

  # Makes the "universe" of the compliment [x_min_start, x_max_end]. Otherwise,
  # setdiff excludes segments of x outside of [min(y_starts), max(y_ends)].
  x_min_start <- min(x_starts)
  x_max_end <- max(x_ends)
  if (y_starts[[1]] > x_min_start) {
    y_comp_starts <- c(x_min_start, y_comp_starts)
    y_comp_ends <- c(y_starts[[1]], y_comp_ends)
  }
  if (y_ends[[length(y_ends)]] < x_max_end) {
    y_comp_starts <- c(y_ends[[length(y_ends)]], y_comp_starts)
    y_comp_ends <- c(x_max_end, y_comp_ends)
  }

  range_intersect(
    x_starts = x_starts,
    x_ends = x_ends,
    y_starts = y_comp_starts,
    y_ends = y_comp_ends,
    inclusive = FALSE
  )
}

range_within <- function(x_starts, x_ends, y_starts, y_ends) {
  y_order <- order(y_starts)
  identical(
    range_flatten(c(x_starts, y_starts), c(x_ends, y_ends)),
    list(starts = y_starts[y_order], ends = y_ends[y_order])
  )
}

range_bound <- function(starts, ends, left, right) {

  out_of_bounds <- left > ends | starts > right

  starts <- starts[!out_of_bounds]
  if (rlang::is_empty(starts)) {
    return(list(starts = NA_real_, ends = NA_real_))
  }
  ends <- ends[!out_of_bounds]

  starts[starts < left] <- left
  ends[ends > right] <- right

  list(starts = starts, ends = ends)
}

range_bound_lower <- function(starts, ends, left) {
  out_of_bounds <- left > ends
  starts <- starts[!out_of_bounds]
  if (rlang::is_empty(starts)) {
    return(list(starts = NA_real_, ends = NA_real_))
  }
  ends <- ends[!out_of_bounds]
  starts[starts < left] <- left
  list(starts = starts, ends = ends)
}

range_bound_upper <- function(starts, ends, right) {
  out_of_bounds <- starts > right
  starts <- starts[!out_of_bounds]
  if (rlang::is_empty(starts)) {
    return(list(starts = NA_real_, ends = NA_real_))
  }
  ends <- ends[!out_of_bounds]
  ends[ends > right] <- right
  list(starts = starts, ends = ends)
}
