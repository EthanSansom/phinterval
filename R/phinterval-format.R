#' @export
vec_ptype_abbr.phinterval <- function(x, ...) {
  tzone <- get_tzone(x)
  if (tz_is_local(tzone)) tzone <- "local" #nocov
  paste0("phint<", tzone, ">")
}

#' @export
vec_ptype_full.phinterval <- function(x, ...) {
  tzone <- get_tzone(x)
  if (tz_is_local(tzone)) tzone <- "local" #nocov
  paste0("phinterval<", tzone, ">")
}

#' @export
obj_print_data.phinterval <- function(x, max_width = getOption("phinterval.print_max_width"), ...) {
  check_number_whole(max_width, min = 1)
  if (is_empty(x)) {
    return(invisible(x))
  }

  # Truncating prior to formatting as format.phinterval() is slow
  max_print <- getOption("max.print", 9999L)
  if (length(x) > max_print) {
    x_t <- x[seq_len(max_print)]
    out <- set_names(format_impl(x_t, max_width = max_width), names(x_t))
    print(out, quote = FALSE)
    cat(" [ Omitted", length(x) - max_print, "entries ]\n")
    return(invisible(x))
  }

  out <- set_names(format_impl(x, max_width = max_width), names(x))
  print(out, quote = FALSE)
  invisible(x)
}

#' @export
format.phinterval <- function(x, max_width = getOption("phinterval.print_max_width"), ...) {
  check_number_whole(max_width, min = 1)
  format_impl(x, max_width = max_width)
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.phinterval <- function(x, ...) {
  widths <- get_format_widths(field(x, "size"))
  pillar::new_pillar_shaft(
    list(phint = x),
    width = widths$set,
    min_width = widths$terse,
    class = "pillar_shaft_phinterval"
  )
}

#' @export
format.pillar_shaft_phinterval <- function(x, width, ...) {
  out <- format_impl(x$phint, max_width = width, style = TRUE)
  pillar::new_ornament(out, align = "left")
}

# formatters -------------------------------------------------------------------

format_impl <- function(x, max_width, style = FALSE) {
  attr(x, "tzone") <- validate_format_tzone(get_tzone(x), call = caller_env())
  if (is_empty(x)) {
    return(character())
  }

  size <- field(x, "size")
  width <- get_format_widths(size)

  if (max_width >= width$set) {
    format_set(size, phint_starts(x), phint_ends(x))
  } else if (max_width >= width$span) {
    format_span(size, phint_start(x), phint_end(x))
  } else {
    format_terse(size)
  }
}

format_set <- function(size, starts, ends, style = FALSE) {
  out <- paste0("{", map2_chr(
    map(starts, function(starts) format(starts, usetz = FALSE)),
    map(ends, function(ends) format(ends, usetz = FALSE)),
    function(starts, ends) {
      # <hole> elements are initially formatted as `character(0L)`
      paste(starts, ends, sep = "--", collapse = ", ") %0|% ""
    }
  ), "}")
  after_format(out, size, style = style)
}

format_span <- function(size, start, end, style = FALSE) {
  start <- format(start, usetz = FALSE)
  end <- format(end, usetz = FALSE)

  out <- paste0("{", start, "-[", size, "]-", end, "}")
  after_format(gsub("[1]", "", out, fixed = TRUE), size, style = style)
}

format_terse <- function(size, style = FALSE) {
  out <- paste0("<phint[", size, "]>")
  after_format(out, size, style = style)
}

after_format <- function(out, size, style = FALSE) {
  out[size == 0] <- "<hole>"
  out[is.na(size)] <- if (style) pillar::style_na("<NA>") else NA_character_
  out
}

# helpers ----------------------------------------------------------------------

# As <phinterval> is a "superclass" of <Interval>, we accept any <Interval>
# vector, including those with an invalid timezone. The consequences:
# format.phinterval() calls format.POSIXct() on *every element* of the `starts`
# and `ends` lists. On an invalid `tzone`, each of those calls emits a warning.
#
# To avoid this, we instead emit one warning at the top of format.phinterval()
# and then convert invalid timezone to "UTC" prior to format.POSIXct(). The
# invalid timezone will still be displayed by vec_ptype_full(), e.g.
#
# > as_phinterval(as.POSIXct(0, "unknown_zone"))
# <phinterval<unknown_zone>[1]>
# [1] {1970-01-01--1970-01-01}
validate_format_tzone <- function(tzone, call = caller_env()) {
  if (is_valid_tzone(tzone)) {
    return(tzone)
  }

  if (is_string(tzone)) {
    a <- paste("an unrecognized timezone:", str_encode(tzone))
  } else {
    a <- obj_type_friendly(tzone)
  }
  warn(
    c(
      x = paste0('`attr(x, "tzone")` is ', a, "."),
      i = 'Unrecognized timezones are formatted using the timezone: "UTC".',
      i = "Run `tzdb_names()` to see recognized timezones."
    ),
    call = call,
    .frequency = "once", # display once per session
    .frequency_id = "phinterval_warning_invalid_tzone"
  )
  "UTC"
}

get_format_widths <- function(size) {
  # <phinterval> vectors have 3 potential formats
  # - Set:   {YYYY-MM-DD HH:MM:SS--YYYY-MM-DD HH:MM:SS, ...}
  # - Span:  {YYYY-MM-DD HH:MM:SS-[size]-YYYY-MM-DD HH:MM:SS}
  # - Terse: <phint[size]>

  max_size <- max(c(0L, size), na.rm = TRUE)
  size_width <- nchar(as.character(max_size))

  # `datetime_width` is pessimistic, as it assumes that all datetimes will be
  # formatted with "YYYY-MM-DD HH:MM:SS" although they may be formatted as
  # "YYYY-MM-DD" instead if there is no "HH:MM:SS" component (e.g. a Date).
  # This is expensive to calculate, since you need to iterate over the `starts`
  # and `ends` lists.
  digits_secs <- getOption("digits.secs", 0L)
  datetime_width <- 19L
  if (digits_secs) datetime_width <- datetime_width + min(digits_secs, 6L) + 1L

  list(
    set = (2L * max_size) + ((datetime_width + 1L) * 2L * max_size),
    span = (datetime_width * 2L) + 6L + size_width,
    terse = size_width + 9L
  )
}
