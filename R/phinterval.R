setOldClass("phinterval")

# TODO Ethan:
# - make a different display method for tibbles! See https://vctrs.r-lib.org/articles/s3-vector.html#format-method
# - see vctrs::list_of, since you're using lists of a lot https://vctrs.r-lib.org/reference/list_of.html
# - change all references to an "Interval" to use `int` for consistency
# - review the `clock` package to see if they have any cool ideas for `phinterval`
#
# - anywhere that you compare two `phinterval`s AND recycling is not automatic
#   (i.e. if you need to use `map`) implement base recycling rules + a custom
#   message
#   - see https://vctrs.r-lib.org/articles/type-size.html#appendix-recycling-in-base-r for discussion of recycling rules

# phinterval class -------------------------------------------------------------
new_phinterval <- function(
    reference_time = lubridate::POSIXct(tz = "UTC"),
    range_starts = list(),
    range_ends = list(),
    tzone = attr(reference_time, "tzone")
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
  if (!is_list_of_dbl(range_starts)) {
    bullets <- c(
      bullets,
      "{.var range_starts} must be a list of double vectors"
    )
  }
  if (!is_list_of_dbl(range_ends)) {
    bullets <- c(
      bullets,
      "{.var range_ends} must be a list of double vectors."
    )
  }
  if (!rlang::is_scalar_character(tzone)) {
    bullets <- c(
      bullets,
      "{.var tzone} must be a scalar character vector."
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

NA_phinterval <- function(n = 1L, tzone = "UTC") {

  new_phinterval(
    reference_time = rep(NA_POSIXct_, n),
    range_starts = as.list(rep(NA_real_, n)),
    range_ends = as.list(rep(NA_real_, n)),
    tzone = tzone
  )

}


# TODO Ethan: This displays nothing if using the local timezone (when tzone == ""),
#             wheras lubridate displays the correct timezone. FIXME

#' @export
format.phinterval <- function(x, ...) {

  reference_time <- field(x, "reference_time")
  range_starts <- field(x, "range_starts")
  range_ends <- field(x, "range_ends")
  tzone <- attr(x, "tzone")

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

vec_ptype2.phinterval.phinterval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

vec_cast.phinterval.phinterval <- function(x, to, ...) x

vec_ptype2.phinterval.Interval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

vec_ptype2.Interval.phinterval <- function(x, y, ...) new_phinterval(tzone = tz_union(x, y))

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

  # `tz` doesn't get the timezone of an `Interval`
  tzone <- tzone %||% lubridate::tz(lubridate::int_start(x))
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

phinterval <- function(intervals = NULL, tzone = NULL) {

  if (is.null(intervals)) {
    tzone <- tzone %||% "UTC"
    stop_wrong_class(tzone, "character", n = 1)
    return(new_phinterval(tzone = tzone))
  }

  stop_not_list_of(intervals, "Interval")

  # Defaulting to the timezone of the first Interval supplied
  tzone <- tzone %||% lubridate::tz(lubridate::int_start(intervals[[1]]))
  stop_wrong_class(tzone, "character", n = 1)

  ints <- map(intervals, lubridate::int_standardize)

  min_start_time <- map_dbl(ints, \(int) min(lubridate::int_start(int)))
  reference_time <- min_start_time |>
    lubridate::as_datetime() |>
    lubridate::with_tz(tzone)

  range_starts <- map2(
    ints, min_start_time,
    \(int, min_start) { as.double(lubridate::int_start(int)) - min_start }
  )
  range_ends <- map2(
    ints, min_start_time,
    \(int, min_start) { as.double(lubridate::int_end(int)) - min_start }
  )

  na_at <- map_lgl(ints, \(int) any(is.na(int)))
  reference_time[na_at] <- lubridate::NA_POSIXct_
  range_starts[na_at]   <- NA_real_
  range_ends[na_at]     <- NA_real_

  ranges <- flatten_overlapping_ranges(range_starts, range_ends)

  new_phinterval(
    reference_time = reference_time,
    range_starts = ranges$starts,
    range_ends = ranges$ends,
    tzone = tzone
  )

}

flatten_overlapping_ranges <- function(range_starts, range_ends) {

  contains_overlaps <- map2_lgl(range_starts, range_ends, range_contains_overlaps)
  contains_overlaps[is.na(contains_overlaps)] <- FALSE

  if (any(contains_overlaps)) {
    range_unions <- map2(
      range_starts[contains_overlaps],
      range_ends[contains_overlaps],
      range_flatten
    )
    range_starts[contains_overlaps] <- map(range_unions, `[[`, "starts")
    range_ends[contains_overlaps] <- map(range_unions, `[[`, "ends")
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
  na_at <- is.na(reference_time)
  seconds <- as.double(reference_time)[!na_at]
  starts <- range_starts[!na_at]
  ends   <- range_ends[!na_at]

  starts_uid[!na_at] <- map2(seconds, starts, \(t, s) t + sort(s)) |> map_chr(range_to_uid)
  ends_uid[!na_at]   <- map2(seconds, ends, \(t, e) t + sort(e)) |> map_chr(range_to_uid)

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
  duration_seconds <- map2(range_starts, range_ends, \(s, e) sum(e - s))

  # `phinterval`s are ordered by start, end, and duration.
  data.frame(
    min_start = min_start,
    max_end = max_end,
    duration_seconds = duration_seconds
  )

}

vec_arith.phinterval <- function(op, x, y, ...) {
  UseMethod("vec_arith.phinterval", y)
}

vec_arith.phinterval.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

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

# TODO Ethan:
# - implement phinterval methods for `with_tz` and `force_tz` from `lubridate`
# - https://github.com/tidyverse/lubridate/blob/e0b50c1759fe35e90a094c012e0c2ce60d47500d/R/time-zones.r#L102
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

  out <- as.list(rep(NA_POSIXct_, length(reference_time)))
  na_at <- is.na(reference_time)

  out[!na_at] <- map2(
    reference_time[!na_at],
    range_starts[!na_at],
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

  out <- as.list(rep(NA_POSIXct_, length(reference_time)))
  na_at <- is.na(reference_time)
  out[!na_at] <- map2(
    reference_time[!na_at],
    range_ends[!na_at],
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

  na_at <- is.na(phint)
  out <- as.list(rep(NA, length(phint)))
  out[!na_at] <- map2(
    range_starts[!na_at],
    range_ends[!na_at],
    \(s, e) sort(e - s)
  )
  out

}

phint_invert <- function(phint) {

  if (lubridate::is.interval(phint)) {
    tzone <- lubridate::tz(lubridate::int_start(phint)[[1]])
    return(NA_phinterval(n = length(phint), tzone = tzone))
  }

  phint <- check_is_phinty(phint)
  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- attr(phint, "tzone")

  is_holey <- !(is.na(reference_time) | map_lgl(range_starts, \(s) length(s) == 1))

  holey_ranges <- order_ranges(range_starts[is_holey], range_ends[is_holey])
  hole_starts  <- map(holey_ranges$ends, \(e) e[-length(e)])
  hole_ends    <- map(holey_ranges$starts, \(s) s[-1L])

  reference_time[!is_holey] <- lubridate::NA_POSIXct_

  new_starts <- as.list(rep(NA_real_, length(reference_time)))
  new_ends   <- as.list(rep(NA_real_, length(reference_time)))
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
  tzone <- attr(phint, "tzone")

  int_starts <- map2(reference_time, range_starts, `+`)
  int_ends   <- map2(reference_time, range_ends, `+`)
  map2(int_starts, int_ends, lubridate::interval, tzone = tzone)

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
    tzone = attr(phint, "tzone")
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
  tzone <- attr(phint, "tzone")

  reference_time <- rep(.POSIXct(0, tz = tzone), length(phint))
  range_starts <- map(phint_starts(phint), \(dates) as.numeric(dates + by))
  range_ends   <- map(phint_ends(phint), \(dates) as.numeric(dates + by))

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

# set operations ---------------------------------------------------------------

phint_squash <- function(phint, na.rm = TRUE) {

  stop_wrong_class(na.rm, "logical", n = 1L)

  phint <- check_is_phinty(phint)
  tzone <- attr(phint, "tzone")

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
    return(NA_phinterval(tzone = tzone))
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

  if (is_phinterval(int)) {
    cli::cli_abort(
      c(
        "{.arg int} must be an {.cls Interval} vector, not a {.cls phinterval}.",
        i = "To squash a {.cls phinterval}, use {.fn phint_squash}."
      )
    )
  }

  stop_wrong_class(na.rm, "logical", n = 1L)
  stop_wrong_class(int, "Interval")

  int <- lubridate::int_standardize(int)
  int_starts <- lubridate::int_start(int)
  int_ends   <- lubridate::int_end(int)
  tzone <- lubridate::tz(int_starts)

  if (rlang::is_empty(int)) {
    return(phinterval(tzone = tzone))
  }

  na_at <- is.na(int)
  if (all(na_at) || (any(na_at) && !na.rm)) {
    return(NA_phinterval(tzone = tzone))
  }
  if (na.rm) {
    int_starts <- int_starts[!na_at]
    int_ends   <- int_ends[!na_at]
  }

  flat_ranges <- range_flatten(as.double(int_starts), as.double(int_ends))
  new_phinterval(
    reference_time = lubridate::POSIXct(1L, tz = tzone),
    range_starts = list(flat_ranges$starts),
    range_ends = list(flat_ranges$ends),
    tzone = tzone
  )

}

phint_overlaps <- function(phint1, phint2, instants = FALSE) {

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
    instants = instants
  )

}

phint_union <- function(phint1, phint2) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_union
  )
}

phint_intersect <- function(phint1, phint2, instants = FALSE) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_intersect,
    instants = instants
  )
}

phint_diff <- function(phint1, phint2, instants = FALSE) {
  combine_phintervals(
    .phint1 = phint1,
    .phint2 = phint2,
    .f = range_setdifference,
    instants = instants
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
  reference_time <- lubridate::POSIXct(out_length, tz = tzone)
  range_starts <-   as.list(rep(NA_real_, out_length))
  range_ends <-     as.list(rep(NA_real_, out_length))

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

standardize_phinterval <- function(phint) {

  reference_time <- field(phint, "reference_time")
  range_starts <- field(phint, "range_starts")
  range_ends <- field(phint, "range_ends")
  tzone <- attr(phint, "tzone")

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
