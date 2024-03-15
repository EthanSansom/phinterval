setOldClass("phinterval")

# TODO Ethan:
# - make a different display method for tibbles! See https://vctrs.r-lib.org/articles/s3-vector.html#format-method
# - see vctrs::list_of, since you're using lists of a lot https://vctrs.r-lib.org/reference/list_of.html
# - review the `clock` package to see if they have any cool ideas for `phinterval`
#
# - anywhere that you compare two `phinterval`s AND recycling is not automatic
#   (i.e. if you need to use `map`) implement base recycling rules + a custom
#   message
#   - see https://vctrs.r-lib.org/articles/type-size.html#appendix-recycling-in-base-r for discussion of recycling rules

# phinterval class -------------------------------------------------------------
new_phinterval <- function(
    reference_time = lubridate::POSIXct(tz = tzone),
    range_starts = list(),
    range_ends = list(),
    tzone = "UTC"
) {

  bullets <- character()
  if (!lubridate::is.POSIXct(reference_time)) {
    bullets <- c(
      bullets,
      paste0(
        "{.var reference_time} must be a {.cls {POSIXct}}, ",
        "not a {.cls {class(reference_time)}"
      )
    )
  }
  if (!(is.list(range_starts) && all(map_lgl(range_starts, is.numeric)))) {
    bullets <- c(
      bullets,
      "{.var range_starts} must be a list of numeric vectors"
    )
  }
  if (!(is.list(range_ends) && all(map_lgl(range_ends, is.numeric)))) {
    bullets <- c(
      bullets,
      "{.var range_ends} must be a list of numeric vectors."
    )
  }
  if (!rlang::is_scalar_character(tzone)) {
    bullets <- c(
      bullets,
      "{.var tzone} must be a scalar character vector."
    )
  }
  if (lubridate::tz(reference_time) != tzone) {
    bullets <- c(
      bullets,
      "{.var reference_time} must be have timezone {.var tzone}."
    )
  }
  if (!rlang::is_empty(bullets)) {
    cli::cli_abort(
      c(
        "Attempted to create malformed {.cls phinterval}.",
        setNames(bullets, rep("x", length(bullets)))
      )
    )
  }

  vctrs::new_rcrd(
    fields = list(
      reference_time = reference_time,
      range_starts = range_starts,
      range_ends = range_ends
    ),
    tzone = tzone,
    class = "phinterval"
  )

}

na_phinterval <- function(n = 1L, tzone = "UTC") {

  new_phinterval(
    reference_time = rep(NA_POSIXct_, n),
    range_starts = as.list(rep(NA_real_, n)),
    range_ends = as.list(rep(NA_real_, n)),
    tzone = tzone
  )

}

na_posixct <- function(n = 1L, tzone = "UTC") {
  as.POSIXct(rep(NA, n), tz = tzone)
}

origin_posixct <- function(n = 1L, tzone = "UTC") {
  lubridate::POSIXct(n, tz = tzone)
}

#' @export
format.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")
  tzone <- attr(x, "tzone")

  # Hack to get the local timezone abbreviation - not sure how else to do this
  tzone <- if (tzone == "") strftime(x = .POSIXct(0L), format = "%Z") else tzone

  starts_order <- map(range_starts, order)
  interval_starts <-
    map2(reference_time, range_starts, `+`) |>
    map2(starts_order, `[`) |>
    map(format, usetz = FALSE)
  interval_ends <-
    map2(reference_time, range_ends, `+`) |>
    map2(starts_order, `[`) |>
    map(format, usetz = FALSE)

  out <- paste0(
    "[",
    map2_chr(
      interval_starts,
      interval_ends,
      \(x, y) paste(x, y, sep = "--", collapse = ", ")
    ),
    "] ",
    tzone
  )
  out[is.na(reference_time)] <- NA_character_
  out

}

vec_ptype_abbr.phinterval <- function(x, ...) {
  "phintrvl"
}

vec_ptype2.phinterval.phinterval <- function(x, y, ...){
  new_phinterval(tzone = tz_union(x, y))
}

vec_ptype2.phinterval.Interval <- function(x, y, ...) {
  new_phinterval(tzone = tz_union(x, y))
}

vec_ptype2.Interval.phinterval <- function(x, y, ...) {
  new_phinterval(tzone = tz_union(x, y))
}

vec_cast.phinterval.phinterval <- function(x, to, ...) x

vec_cast.phinterval.Interval <- function(x, to, ...) as_phinterval(x)

is_phinterval <- function(x) {
  inherits(x, "phinterval")
}

as_phinterval <- function(x, ...) {
  UseMethod("as_phinterval")
}

as_phinterval.default <- function(x, ...) {
  vec_cast(x, new_phinterval())
}

as_phinterval.Interval <- function(x, tzone = NULL) {

  tzone <- tzone %||% interval_tzone(x)
  stop_wrong_class(tzone, "character", n = 1L)

  int <- lubridate::int_standardize(x)
  na_at <- is.na(int)

  reference_time <- lubridate::int_start(int)
  range_starts   <- as.list(rep(0, length(int)))
  range_ends     <- as.list(as.double(lubridate::int_length(int)))

  reference_time[na_at] <- lubridate::NA_POSIXct_
  range_starts[na_at]   <- NA_real_
  range_ends[na_at]     <- NA_real_

  new_phinterval(
    reference_time = reference_time,
    range_starts = range_starts,
    range_ends = range_ends,
    tzone = tzone
  )

}

# TODO: Check for invalid timezones at the VERY start. Maybe just make an invalid
#       timezone function. I think you'll have to use `timechange:::C_valid_tz`.
#       Invalid timezones don't cause an error with empty phinterval, but they
#       DO cause an error with a non-empty phinterval.
#
# See: https://github.com/tidyverse/lubridate/blob/main/R/time-zones.r
phinterval <- function(intervals = NULL, tzone = NULL) {

  if (rlang::is_empty(intervals)) {
    tzone <- tzone %||% "UTC"
    stop_wrong_class(tzone, "character", n = 1)
    return(new_phinterval(tzone = tzone))
  }

  stop_not_list_of(intervals, "Interval")

  # Defaulting to the timezone of the first Interval supplied
  tzone <- tzone %||% interval_tzone(intervals[[1]])
  stop_wrong_class(tzone, "character", n = 1)
  ints <- map(intervals, lubridate::int_standardize)

  # If intervals[[i]] is empty, setting [[i]] to NA. Removing empty intervals
  # would make the output length harder to predict
  non_na_at <- !map_lgl(ints, \(int) rlang::is_empty(int) || any(is.na(int)))
  non_na_ints <- ints[non_na_at]

  reference_time <- na_posixct(length(ints), tzone = tzone)
  range_starts <- as.list(rep(NA_real_, length(ints)))
  range_ends <- range_starts

  min_start_time <- map_dbl(non_na_ints, \(int) min(lubridate::int_start(int)))
  reference_time[non_na_at] <- lubridate::as_datetime(min_start_time)
  range_starts[non_na_at] <- map2(
    non_na_ints,
    min_start_time,
    \(int, min_start) { as.double(lubridate::int_start(int)) - min_start }
  )
  range_ends[non_na_at] <- map2(
    non_na_ints,
    min_start_time,
    \(int, min_start) { as.double(lubridate::int_end(int)) - min_start }
  )

  ranges <- flatten_overlapping_ranges(range_starts, range_ends)

  new_phinterval(
    reference_time = reference_time,
    range_starts = ranges$starts,
    range_ends = ranges$ends,
    tzone = tzone
  )

}

# TODO: Consolidate the `range_is_flat` and `range_contains_overlaps` functions.
#       Think about the best language to describe overlapping vs. flat vs. intersecting
flatten_overlapping_ranges <- function(range_starts, range_ends) {

  range_not_flat <- !map2_lgl(range_starts, range_ends, range_is_flat)
  range_not_flat[is.na(range_not_flat)] <- FALSE

  if (any(range_not_flat)) {
    flattened <- map2(
      range_starts[range_not_flat],
      range_ends[range_not_flat],
      range_flatten
    )
    range_starts[range_not_flat] <- map(flattened, `[[`, "starts")
    range_ends[range_not_flat] <- map(flattened, `[[`, "ends")
  }

  list(starts = range_starts, ends = range_ends)

}

# `phinterval`s are equal when they represent the same collection of time-spans,
# ignoring timezone. This is unlike `Interval` equality, which is determined by
# duration of the time-span. https://github.com/tidyverse/lubridate/issues/1135

#' @export
vec_proxy_equal.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")

  starts_uid <- rep(NA_character_, length(reference_time))
  ends_uid   <- starts_uid

  # TODO: This feels illegal, but should (?) uniquely ID a set of time spans
  non_na_at <- !is.na(reference_time)
  seconds <- as.double(reference_time)[non_na_at]
  starts <- range_starts[non_na_at]
  ends   <- range_ends[non_na_at]

  starts_uid[non_na_at] <- map2(seconds, starts, \(t, s) t + sort(s)) |> map_chr(range_to_uid)
  ends_uid[non_na_at]   <- map2(seconds, ends, \(t, e) t + sort(e)) |> map_chr(range_to_uid)

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

# division ---------------------------------------------------------------------

divide_phinterval_by_duration <- function(phint, dur) {
  as.duration(phint) / dur
}

divide_phinterval_by_difftime <- function(phint, diff) {
  as.duration(phint) / diff
}

divide_phinterval_by_period <- function(phint, per) {
  map2_dbl(phint_to_spans(phint), per, \(spans, p) sum(spans / p))
}

trunc_divide <- function(e1, e2) trunc(e1 / e2)

#' @export
setMethod(
  "/", signature(e1 = "phinterval", e2 = "Duration"),
  function(e1, e2) divide_phinterval_by_duration(e1, e2)
)

#' @export
setMethod(
  "/", signature(e1 = "phinterval", e2 = "difftime"),
  function(e1, e2) divide_phinterval_by_difftime(e1, e2)
)

#' @export
setMethod(
  "/", signature(e1 = "phinterval", e2 = "Period"),
  function(e1, e2) divide_phinterval_by_period(e1, e2)
)

#' @export
setMethod("%/%", signature(e1 = "phinterval", e2 = "Duration"), trunc_divide)

#' @export
setMethod("%/%", signature(e1 = "phinterval", e2 = "difftime"), trunc_divide)

#' @export
setMethod("%/%", signature(e1 = "phinterval", e2 = "Period"), trunc_divide)

# phinterval interface ---------------------------------------------------------

# `%within%` follows `base` recycling rules in `lubridate`, opposed to `tidyverse`
# recycling https://vctrs.r-lib.org/reference/theory-faq-recycling.html. Following
# `lubridate` approach here.
#
# `"instant" %within% "list of Interval"` is defined in `lubridate`, but I don't
# want to mess with the `signature(a = , b = "list")` definitions. So, not
# defining a "list of phinterval" version here.

setMethod("%within%", signature(a = "phinterval", b = "phinterval"), function(a, b) {
  within_phinterval(a, b)
})

setMethod("%within%", signature(a = "Interval", b = "phinterval"), function(a, b) {
  int <- as_phinterval(a)
  within_phinterval(int, b)
})

setMethod("%within%", signature(a = "phinterval", b = "Interval"), function(a, b) {
  int <- as_phinterval(b)
  within_phinterval(a, int)
})

setMethod("%within%", signature(b = "phinterval"), function(a, b) {
  if (!lubridate::is.instant(a)) {
    cli::cli_abort(
      c(
        "{.arg a} must be a valid datetime vector, not {.cls {class(a)}.",
        i = "Valid datetimes are those which pass {.fn lubridate::is.instant}."
      )
    )
  }
  a <- as.POSIXct(a)
  within_instant(a, b)
})

within_phinterval <- function(x_phint, y_phint) {

  x_range <- rangify_phinterval(x_phint)
  y_range <- rangify_phinterval(y_phint)

  out <- rep(NA, length(x_phint))
  non_na_at <- !(is.na(x_phint) | is.na(y_phint))

  out[non_na_at] <- pmap_lgl(
    list(
      x_starts = x_range$starts[non_na_at],
      x_ends = x_range$ends[non_na_at],
      y_starts = y_range$starts[non_na_at],
      y_ends = y_range$ends[non_na_at]
    ),
    range_within
  )
  out

}

within_instant <- function(instant, phint) {

  out <- rep(NA, length(phint))
  non_na_at <- !(is.na(instant) | is.na(phint))

  if (!any(non_na_at)) {
    return(out)
  }

  instant <- instant[non_na_at]
  phint <- phint[non_na_at]

  in_starts <- map2(instant, phint_starts(phint), `>=`)
  in_ends   <- map2(instant, phint_ends(phint), `<=`)

  out[non_na_at] <- map2_lgl(in_starts, in_ends, \(in_s, in_e) any(in_s & in_e))
  out

}

# TODO: Export this method appropriately
tz.phinterval <- function(x) {
  attr(x, "tzone")
}

# TODO Ethan:
# See https://stackoverflow.com/questions/31317366/in-r-how-can-i-extend-generic-methods-from-one-package-in-another
# I'm not sure of the right way to extend a generic. Check R Packages for details (?)
# or the S3 chapter of Advanced R
setGeneric("as.duration", getGeneric("as.duration", package="lubridate"))
setMethod("as.duration", signature(x = "phinterval"), function(x) {
  as.duration(phint_length(x))
})

is_holey <- function(phint) {

  if (lubridate::is.interval(phint)) {
    out <- rep(FALSE, length(phint))
  } else{
    phint <- check_is_phinty(phint)
    range_starts <- field(phint, "range_starts")
    out <- map_lgl(range_starts, \(s) length(s) > 1)
  }

  out[is.na(phint)] <- NA
  out

}

n_spans <- function(phint) {

  if (lubridate::is.interval(phint)) {
    out <- rep(1L, length(phint))
  } else{
    phint <- check_is_phinty(phint)
    range_starts <- field(phint, "range_starts")
    out <- map_int(range_starts, length)
  }

  out[is.na(phint)] <- NA_integer_
  out

}

n_holes <- function(phint) {

  n_spans <- n_spans(phint)
  n_spans - 1L

}

phint_start <- function(phint) {

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  reference_time + map_dbl(range_starts, min)

}

phint_starts <- function(phint) {

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")

  out <- as.list(na_posixct(length(phint), tzone = tz(phint)))
  non_na_at <- !is.na(phint)

  out[non_na_at] <- map2(
    reference_time[non_na_at],
    range_starts[non_na_at],
    \(t, s) t + sort(s)
  )
  out

}

phint_end <- function(phint) {

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_ends <- field(phint, "range_ends")
  reference_time + map_dbl(range_ends, max)

}

phint_ends <- function(phint) {

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_ends <- field(phint, "range_ends")

  out <- as.list(na_posixct(length(phint), tzone = tz(phint)))
  non_na_at <- !is.na(phint)

  out[non_na_at] <- map2(
    reference_time[non_na_at],
    range_ends[non_na_at],
    \(t, e) t + sort(e)
  )
  out

}

phint_length <- function(phint) {

  map_dbl(phint_lengths(phint), sum)

}

phint_lengths <- function(phint) {

  phint <- check_is_phinty(phint)
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")

  out <- as.list(rep(NA_real_, length(phint)))
  non_na_at <- !is.na(phint)

  out[non_na_at] <- map2(
    range_starts[non_na_at],
    range_ends[non_na_at],
    \(s, e) sort(e - s)
  )
  out

}

phint_invert <- function(phint) {

  if (lubridate::is.interval(phint)) {
    tzone <- interval_tzone(phint)
    return(na_phinterval(n = length(phint), tzone = tzone))
  }

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- tz(phint)

  is_holey <- !(is.na(phint) | map_lgl(range_starts, \(s) length(s) == 1))

  holey_ranges <- order_ranges(range_starts[is_holey], range_ends[is_holey])
  hole_starts  <- map(holey_ranges$ends, \(e) e[-length(e)])
  hole_ends    <- map(holey_ranges$starts, \(s) s[-1L])

  reference_time[!is_holey] <- na_posixct(1L, tzone = tzone)

  new_starts <- as.list(rep(NA_real_, length(phint)))
  new_ends   <- new_starts
  new_starts[is_holey] <- hole_starts
  new_ends[is_holey]   <- hole_ends

  new_phinterval(
    reference_time = reference_time,
    range_starts = new_starts,
    range_ends = new_ends,
    tzone = tzone
  )

}

order_ranges <- function(range_starts, range_ends) {

  starts_order <- map(range_starts, order)
  starts <- map2(range_starts, starts_order, \(s, o) s[o])
  ends <- map2(range_ends, starts_order, \(e, o) e[o])
  list(starts = starts, ends = ends)

}

phint_to_spans <- function(phint) {

  phint <- check_is_phinty(phint)
  phint <- standardize_phinterval(phint)

  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- tz(phint)

  int_starts <- map2(reference_time, range_starts, `+`)
  int_ends   <- map2(reference_time, range_ends, `+`)
  map2(int_starts, int_ends, interval, tzone = tzone)

}

phint_to_holes <- function(phint) {

  phint |>
    check_is_phinty() |>
    phint_invert() |>
    phint_to_spans()

}

phint_shift <- function(phint, by, on = c("all", "start")) {

  on <- rlang::arg_match(on)
  if (on == "all") {
    return(phint_shift_faithful(phint, by))
  }

  if (!lubridate::is.timespan(by)) {
    cli::cli_abort("{.arg by} must be a timespan, not a {.cls {class(by)}.")
  }
  if (lubridate::is.interval(by)) {
    cli::cli_abort("{.arg by} can't be a {.cls Interval}.")
  }

  # `lubridate` uses the recycling rules of `+` to recycle both the time-span
  # and the shift `by`. Using `recycle2_common` to match this approach.
  objs  <- recycle2_common(phint, by)
  phint <- standardize_phinterval(check_is_phinty(objs$x))
  by    <- objs$y

  new_phinterval(
    reference_time = field(phint, "reference_time") + by,
    range_starts = field(phint, "range_starts"),
    range_ends = field(phint, "range_ends"),
    tzone = tz(phint)
  )

}

# This matches the `lubridate` implementation of `int_shift. In particular,
# `is.na(phint_shift_faithful(int)) == is.na(int_shift(int))`.
phint_shift_faithful <- function(phint, by) {

  if (!lubridate::is.timespan(by)) {
    cli::cli_abort("{.arg by} must be a timespan, not a {.cls {class(by)}.")
  }
  if (lubridate::is.interval(by)) {
    cli::cli_abort("{.arg by} can't be a {.cls Interval}.")
  }

  objs  <- recycle2_common(phint, by)
  phint <- check_is_phinty(objs$x)
  by    <- objs$y
  tzone <- tz(phint)

  reference_time <- rep(.POSIXct(0, tz = tzone), length(phint))
  range_starts <- map(phint_starts(phint), \(dates) as.double(dates + by))
  range_ends   <- map(phint_ends(phint), \(dates) as.double(dates + by))

  na_at <- map2_lgl(range_starts, range_ends, \(s, e) any(is.na(s) | is.na(e)))
  reference_time[na_at] <- lubridate::NA_POSIXct_
  range_starts[na_at]   <- NA_real_
  range_ends[na_at]     <- NA_real_

  new_phinterval(
    reference_time = reference_time,
    range_starts = range_starts,
    range_ends = range_ends,
    tzone = tzone
  )

}

phint_bound <- function(phint, left = NULL, right = NULL) {

  phint <- check_is_phinty(phint)
  phint_len <- length(phint)

  left_null  <- is.null(left)
  right_null <- is.null(right)
  if (left_null && right_null) {
    cli::cli_abort("Must supply at least one of {.arg left} or {.arg right}.")
  }
  if (phint_len == 0) {
    return(phint)
  }
  if (!left_null) {
    stop_wrong_class(left, c("Date", "POSIXct", "POSIXlt"))
    left <- recycle_to(left, phint)
  }
  if (!right_null) {
    stop_wrong_class(right, c("Date", "POSIXct", "POSIXlt"))
    right <- recycle_to(right, phint)
  }
  if (!(left_null | right_null)) {
    left_after_right <- map2_lgl(left, right, \(l, r) l > r)
    if (isTRUE(any(left_after_right))) {
      error_index <- which.max(left_after_right)
      cli::cli_abort(
        c(
          "{.arg left} bound must be less than or equal to {.arg right}.",
          x = "Recycled {.code left > right} at index {error_index}.",
          i = "`left[[{error_index}]]` is {.val {left[[error_index]]}}.",
          i = "`right[[{error_index}]]` is {.val {right[[error_index]]}}."
        )
      )
    }
  }

  tzone <- tz(phint)
  reference_time <- origin_posixct(phint_len, tzone = tzone)
  range_starts <- as.list(rep(NA_real_, phint_len))
  range_ends <- range_starts

  range_left  <- as.double(as.POSIXct(left))
  range_right <- as.double(as.POSIXct(right))
  range <- rangify_phinterval(phint)

  if (left_null) {
    # TODO Ethan: Implement `out_of_bounds` check. We want to return NA
    #             for `phinterval` that is outside of the provided [left, right]
    #             bounds!
    #
    #             Actually, that's super annoying. Just implement this in the
    #             three `range_bound*` functions
    non_na_at <- !(is.na(right) | is.na(phint))
    range_bounded <- pmap(
      list(
        starts = range$starts[non_na_at & !out_of_bounds],
        ends = range$ends[non_na_at & !out_of_bounds],
        right = range_right[non_na_at & !out_of_bounds]
      ),
      range_bound_upper
    )
  } else if (right_null) {
    non_na_at <- !(is.na(left) | is.na(phint))
    range_bounded <- pmap(
      list(
        starts = range$starts[non_na_at],
        ends = range$ends[non_na_at],
        left = range_left[non_na_at]
      ),
      range_bound_lower
    )
  } else {
    non_na_at <- !(is.na(left) | is.na(right) | is.na(phint))
    range_bounded <- pmap(
      list(
        starts = range$starts[non_na_at],
        ends = range$ends[non_na_at],
        left = range_left[non_na_at],
        right = range_right[non_na_at]
      ),
      range_bound
    )
  }

  range_starts[non_na_at] <- map(range_bounded, `[[`, "starts")
  range_ends[non_na_at] <- map(range_bounded, `[[`, "ends")
  reference_time[is.na(range_starts)] <- na_posixct(1L, tzone = tzone)

  new_phinterval(
    reference_time = reference_time,
    range_starts = range_starts,
    range_ends = range_ends,
    tzone = tzone
  )

}

# set operations ---------------------------------------------------------------

# TODO Ethan: You might want an option to return NA on empty, so
#             that you can use this in a `summarize` statement
#             and reliably return a length-1 output
phint_squash <- function(phint, na.rm = TRUE) {

  stop_wrong_class(na.rm, "logical", n = 1L)

  if (lubridate::is.interval(phint)) {
    return(int_squash(phint, na.rm = na.rm))
  }

  phint <- check_is_phinty(phint)
  tzone <- tz(phint)

  if (rlang::is_empty(phint)) {
    return(phinterval(tzone = tzone))
  }

  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")

  reference_seconds <- as.double(reference_time)
  span_starts <- map2(reference_seconds, range_starts, `+`)
  span_ends   <- map2(reference_seconds, range_ends, `+`)

  na_at <- is.na(reference_time)
  if (all(na_at) || (any(na_at) && !na.rm)) {
    return(na_phinterval(tzone = tzone))
  }
  if (na.rm) {
    span_starts <- span_starts[!na_at]
    span_ends   <- span_ends[!na_at]
  }

  flat_ranges <- range_flatten(list_c(span_starts), list_c(span_ends))
  new_phinterval(
    reference_time = lubridate::POSIXct(1L, tz = tzone),
    range_starts = list(flat_ranges$starts),
    range_ends = list(flat_ranges$ends),
    tzone = tzone
  )

}

int_squash <- function(int, na.rm = TRUE) {

  int <- lubridate::int_standardize(int)
  int_starts <- lubridate::int_start(int)
  int_ends <- lubridate::int_end(int)
  tzone <- lubridate::tz(int_starts)

  if (rlang::is_empty(int)) {
    return(phinterval(tzone = tzone))
  }

  na_at <- is.na(int)
  if (all(na_at) || (any(na_at) && !na.rm)) {
    return(na_phinterval(tzone = tzone))
  }
  if (na.rm) {
    int_starts <- int_starts[!na_at]
    int_ends <- int_ends[!na_at]
  }

  flat_ranges <- range_flatten(as.double(int_starts), as.double(int_ends))
  new_phinterval(
    reference_time = origin_posixct(1L, tzone = tzone),
    range_starts = list(flat_ranges$starts),
    range_ends = list(flat_ranges$ends),
    tzone = tzone
  )

}

phint_overlaps <- function(phint1, phint2, inclusive = FALSE) {

  objs <- recycle2_common(phint1, phint2)
  phint1 <- check_is_phinty(objs$x)
  phint2 <- check_is_phinty(objs$y)

  range1 <- rangify_phinterval(phint1)
  range2 <- rangify_phinterval(phint2)

  non_na_at <- !(is.na(phint1) | is.na(phint2))

  range1_starts <- range1$starts[non_na_at]
  range1_ends   <- range1$ends[non_na_at]
  range2_starts <- range2$starts[non_na_at]
  range2_ends   <- range2$ends[non_na_at]

  out <- rep(NA, length(phint1))
  out[non_na_at] <- pmap_lgl(
    list(
      x_starts = range1_starts,
      x_ends = range1_ends,
      y_starts = range2_starts,
      y_ends = range2_ends
    ),
    range_intersects,
    inclusive = inclusive
  )

}

phint_union <- function(phint1, phint2) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_union
  )
}

phint_intersect <- function(phint1, phint2, inclusive = FALSE) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_intersect,
    inclusive = inclusive
  )
}

phint_diff <- function(phint1, phint2) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_setdifference
  )
}

# helpers ----------------------------------------------------------------------

combine_phintervals <- function(.phint1, .phint2, .f, ...) {

  objs <- recycle2_common(.phint1, .phint2)
  phint1 <- check_is_phinty(objs$x)
  phint2 <- check_is_phinty(objs$y)

  range1 <- rangify_phinterval(phint1)
  range2 <- rangify_phinterval(phint2)

  non_na_at <- !(is.na(phint1) | is.na(phint2))

  range1_starts <- range1$starts[non_na_at]
  range1_ends   <- range1$ends[non_na_at]
  range2_starts <- range2$starts[non_na_at]
  range2_ends   <- range2$ends[non_na_at]

  out_range <- pmap(
    list(
      x_starts = range1_starts,
      x_ends = range1_ends,
      y_starts = range2_starts,
      y_ends = range2_ends
    ),
    .f = .f,
    ...
  )

  out_length <- length(phint1)

  tzone <-          tz_union(phint1, phint2)
  reference_time <- origin_posixct(out_length, tzone = tzone)
  range_starts <-   as.list(rep(NA_real_, out_length))
  range_ends <-     range_starts

  range_starts[non_na_at] <- map(out_range, `[[`, "starts")
  range_ends[non_na_at] <-   map(out_range, `[[`, "ends")

  new_phinterval(
    reference_time = reference_time,
    range_starts = range_starts,
    range_ends = range_ends,
    tzone = tzone
  )

}

rangify_phinterval <- function(phint) {

  reference_seconds <- as.double(field(phint, "reference_time"))
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")

  list(
    starts = map2(reference_seconds, range_starts, `+`),
    ends = map2(reference_seconds, range_ends, `+`)
  )

}

# TODO Ethan: See if this has any use case
phintify_ranges <- function(starts, ends, tzone) {

  reference_time <- rep(.POSIXct(0L, tz = tzone), length(starts))
  new_phinterval(
    reference_time = reference_time,
    range_starts = starts,
    range_ends = ends,
    tzone = tzone
  )

}

standardize_phinterval <- function(phint) {

  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- tz(phint)

  starts_order <- map(range_starts, order)
  starts_min   <- map_dbl(range_starts, min)

  new_time <- reference_time + starts_min
  new_starts <- pmap(
    list(range_starts, starts_order, starts_min),
    \(range_starts, starts_order, starts_min) {
      range_starts[starts_order] - starts_min
    }
  )
  new_ends <- pmap(
    list(range_ends, starts_order, starts_min),
    \(range_ends, starts_order, starts_min) {
      range_ends[starts_order] - starts_min
    }
  )

  new_phinterval(
    reference_time = new_time,
    range_starts = new_starts,
    range_ends = new_ends,
    tzone = tzone
  )

}
