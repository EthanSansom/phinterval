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
