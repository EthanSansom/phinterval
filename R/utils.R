# time zones -------------------------------------------------------------------

get_tzone <- function(x) {
  UseMethod("get_tzone")
}

get_tzone.Interval <- function(x) {
  lubridate::tz(lubridate::int_start(x))
}

get_tzone.phinterval <- function(x) {
  attr(x, "tzone")
}

get_tzone.default <- function(x) {
  lubridate::tz(x)
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

# miscellaneous ----------------------------------------------------------------

`%0|%` <- function(lhs, rhs) if (is_empty(lhs)) rhs else lhs
