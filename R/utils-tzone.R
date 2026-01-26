#nocov start

# From {lubridate}:
# - https://github.com/tidyverse/lubridate/blob/69034a5d933b53b9d8815f954c877c539aff868a/R/vctrs.R#L212

# `get_tzone()` coerces non-string input to "UTC" for non-phinterval inputs
# - POSIXct can have NA_character_ timezone
# - Don't bother trying to stop `attr(x, "tzone")<-` for an extant <phinterval>
get_tzone <- function(x) {
  UseMethod("get_tzone")
}

#' @export
get_tzone.Interval <- function(x) {
  attr(x, "tzone") %|string% "UTC"
}

#' @export
get_tzone.default <- function(x) {
  lubridate::tz(x) %|string% "UTC"
}

#' @export
get_tzone.phinterval <- function(x) {
  attr(x, "tzone")
}

tz_is_local <- function(x) {
  identical(x, "")
}

tz_union <- function(x, y) {
  x_tzone <- get_tzone(x)
  y_tzone <- get_tzone(y)

  if (tz_is_local(x_tzone)) {
    y_tzone
  } else {
    x_tzone
  }
}

#nocov end
