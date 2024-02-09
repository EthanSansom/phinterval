`%notin%` <- Negate(`%in%`)

# predicates -------------------------------------------------------------------
is_list_of_dbl <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, rlang::is_double))
}

is_list_of_POSIXct <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, rlang::is_double))
}

# time wrangling ---------------------------------------------------------------

# NOTE: Both `tz_is_local` and `tz_union` are stolen directly from `interval`.
#       https://github.com/tidyverse/lubridate/blob/main/R/vctrs.R
tz_is_local <- function(x) {
  identical(x, "")
}

tz_union <- function(x, y) {
  x_tzone <- attr(x, "tzone")
  y_tzone <- attr(y, "tzone")

  if (tz_is_local(x_tzone)) {
    y_tzone
  } else {
    x_tzone
  }
}

# interval helpers -------------------------------------------------------------

intvl_start_dbl  <- function(intvl) as.double(lubridate::int_start(intvl))
intvl_end_dbl    <- function(intvl) as.double(lubridate::int_end(intvl))
intvl_length_dbl <- function(intvl) as.double(lubridate::int_length(intvl))
