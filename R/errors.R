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

  # Hack to ensure that recognized time zones match those in lubridate
  tryCatch(
    lubridate::force_tz(lubridate::origin, tzone = tzone),
    error = function(e) {
      abort(
        sprintf("`%s = %s` is an unrecognized timezone.", arg, str_encode(x, quote = "\"")),
        call = call,
        arg = arg
      )
    }
  )
  return(invisible(NULL))
}

check_is_phintish <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (lubridate::is.interval(x) || is_phinterval(x)) {
    return(invisible(NULL))
  }
  stop_input_type(
    x,
    "a <phinterval> or <Interval> vector",
    arg = arg,
    call = call
  )
}

check_is_list_of_phintish <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is.list(x)) {
    stop_input_type(
      x,
      "a list of <Interval> or <phinterval> vectors",
      arg = arg,
      call = call
    )
  }
  not_phintish <- !map_lgl(x, \(elm) lubridate::is.interval(elm) || is_phinterval(elm))
  if (any(not_phintish)) {
    idx <- which.max(not_phintish)
    arg <- paste0(arg, "[[", idx, "]]")
    stop_input_type(
      x[[idx]],
      "a <phinterval> or <Interval> vector",
      arg = arg,
      call = call
    )
  }
  return(invisible(NULL))
}

str_encode <- function(x, width = 30, ...) {
  if (nchar(x) > width) {
    x <- substr(x, 1, width - 3)
    x <- paste0(x, "...")
  }
  encodeString(x, ...)
}
