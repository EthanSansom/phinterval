#nocov start

check_phintish <- function(x, arg = caller_arg(x), call = caller_env()) {
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

validate_type_phintish <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_phinterval(x)) {
    return("phint")
  } else if (lubridate::is.interval(x)) {
    return("intvl")
  }

  stop_input_type(
    x,
    "a <phinterval> or <Interval> vector",
    arg = arg,
    call = call
  )
}

check_phintish_or_instant <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_phintish(x) || is_instant(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a <phinterval>, <Interval>, or datetime vector",
    arg = arg,
    call = call
  )
}

validate_type_phintish_or_instant <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_phinterval(x)) {
    return("phint")
  } else if (lubridate::is.interval(x)) {
    return("intvl")
  } else if (is_instant(x)) {
    "point"
  }

  stop_input_type(
    x,
    "a <phinterval>, <Interval>, or datetime vector",
    arg = arg,
    call = call
  )
}

check_instant <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_instant(x)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a datetime vector",
    arg = arg,
    call = call
  )
}

check_vector <- function(x, allow_null = TRUE, arg = caller_arg(x), call = caller_env()) {
  if (obj_is_vector(x) || (is.null(x) && allow_null)) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    "a vector",
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
  x_size <- vec_size(x)
  y_size <- vec_size(y)
  if (x_size == y_size || x_size == 1 || y_size == 1) {
    return(invisible(NULL))
  }

  abort(
    sprintf(
      "Can't recycle `%s` (size %i) and `%s` (size %i) to a common size.",
      x_arg, x_size, y_arg, y_size
    ),
    call = call
  )
}

check_recycleable_to <- function(
    x,
    to,
    x_arg = caller_arg(x),
    to_arg = caller_arg(to),
    call = caller_env()
) {
  x_size <- vec_size(x)
  to_size <- vec_size(to)
  if (x_size == to_size || x_size == 1) {
    return(invisible(NULL))
  }

  abort(
    sprintf(
      "Can't recycle `%s` (size %i) to match `%s` (size %i).",
      x_arg, x_size, to_arg, to_size
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
