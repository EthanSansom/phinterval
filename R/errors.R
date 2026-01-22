#nocov start

check_valid_tzone <- function(
    x,
    allow_null = FALSE,
    arg = caller_arg(x),
    call = caller_env()
  ) {
  if (allow_null && is.null(x)) {
    return(invisible(NULL))
  }

  check_string(x, arg = arg, call = call)
  if (!tzone_is_valid_cpp(x)) {
    abort(
      c(
        sprintf("`%s` is an unrecognized timezone: %s.", arg, str_encode(x)),
        i = "Run `tzdb_names()` to see recognized timezones."
      ),
      call = call
    )
  }

  return(invisible(NULL))
}

check_is_phintish <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_phintish(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <phinterval> or <Interval> vector",
    arg = arg,
    call = call
  )
}

check_recycleable <- function(
    x,
    y,
    x_arg = caller_arg(x),
    y_arg = caller_arg(y),
    call = caller_env()
  ) {
  x_len <- length(x)
  y_len <- length(y)
  if (x_len == y_len || x_len == 1 || y_len == 1) {
    return(invisible(NULL))
  }

  abort(
    sprintf(
      "Can't recycle `%s` (length %i) and `%s` (length %i) to a common length.",
      x_arg, x_len, y_arg, y_len
    ),
    call = call
  )
}

str_encode <- function(x, width = 30, quote = "\"", ...) {
  if (nchar(x) > width) {
    x <- substr(x, 1, width - 3)
    x <- paste0(x, "...")
  }
  encodeString(x, quote = quote, ...)
}

#nocov end
