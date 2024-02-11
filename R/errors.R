# TODO:
# - think about nicely designed and re-usable error messages with `cli`. Good for consistency.
#   - Notes on error constructors: https://design.tidyverse.org/err-constructor.html
#   - Notes on error messages: https://style.tidyverse.org/error-messages.html
#   - `rlang::caller_arg` pattern is great: https://rlang.r-lib.org/reference/caller_arg.html

# TODO Ethan: Clean this up to handle the null v. non-null `n` paths
stop_wrong_class <- function(
    x,
    cls,
    n = NULL,
    arg = rlang::caller_arg(x),
    error_call = rlang::caller_env()
  ) {

  if (is.null(n)) {

    if (!inherits(x, cls)) {
      cli::cli_abort(
        paste0(
          "{.arg {arg}} must be a {.cls cls} vector, ",
          "not a {.cls {class(x)}}."
        ),
        arg = arg,
        call = error_call
      )
    }

  } else {

    if (!inherits(x, cls)) {
      len_msg <- if (n == 1) "scalar" else paste0("length-", n)
      cli::cli_abort(
        paste0(
          "{.arg {arg}} must be a {len_msg} {.cls cls} vector, ",
          "not a {.cls {class(x)}}."
        ),
        arg = arg,
        call = error_call
      )
    } else if (length(x) != n) {
      cli::cli_abort(
        c(
          "{.arg {arg}} must have length {n}.",
          i = "{.arg {arg}} has length {length(x)}."
        ),
        arg = arg,
        call = error_call
      )
    }

  }

}

check_is_phinty <- function(
    x,
    arg = rlang::caller_arg(x),
    error_call = rlang::caller_env()
  ) {

  if (is_phinterval(x)) {
    return(x)
  }
  if (lubridate::is.interval(x)) {
    return(as_phinterval(x))
  }

  cli::cli_abort(
    paste0(
      "{.arg {arg}} must be a {.cls phinterval} or a {.cls Interval} vector, ",
      "not a {.cls {class(x)}}."
    ),
    arg = arg,
    call = error_call
  )

}
