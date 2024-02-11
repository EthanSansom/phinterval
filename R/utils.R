# misc -------------------------------------------------------------------------
`%notin%` <- Negate(`%in%`)

# predicates -------------------------------------------------------------------

# TODO Ethan: Check how these behave with NA vectors of other classes, ex. NA_integer
#             Right not I treat these as fine (i.e. is_list_of_dbl is okay with NA_interger),
#             but this might cause problems down-stream. Maybe these list of functions
#             aren't the best move.

is_list_of_dbl <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, \(x) rlang::is_double(x) || all(is.na(x))))
}

is_list_of_POSIXct <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, \(x) lubridate::is.POSIXct(x) || all(is.na(x))))
}

is_list_of_Interval <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, \(x) lubridate::is.interval(x) || all(is.na(x))))
}

# time wrangling ---------------------------------------------------------------

# Both `tz_is_local` and `tz_union` are borrowed directly from lubridate for
# consistency. https://github.com/tidyverse/lubridate/blob/main/R/vctrs.R
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
