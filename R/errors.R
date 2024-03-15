# TODO:
# - think about nicely designed and re-usable error messages with `cli`. Good for consistency.
#   - Notes on error constructors: https://design.tidyverse.org/err-constructor.html
#   - Notes on error messages: https://style.tidyverse.org/error-messages.html
#   - `rlang::caller_arg` pattern is great: https://rlang.r-lib.org/reference/caller_arg.html

stop_wrong_class <- function(
    x,
    cls,
    n = NULL,
    arg = rlang::caller_arg(x),
    error_call = rlang::caller_env()
  ) {

  if (is.null(n)) {

    if (!inherits(x, cls)) {
      correct_cls <- commas(paste0("{.cls ", cls, "}"))
      cli::cli_abort(
        paste0(
          "{.arg {arg}} must be a ", correct_cls, " vector, ",
          "not a {.cls {class(x)}}."
        ),
        arg = arg,
        call = error_call,
        class = "phinterval_error_wrong_class"
      )
    }

    return(invisible())
  }

  if (!inherits(x, cls)) {
    len_msg <- if (n == 1) "scalar" else paste0("length-", n)
    correct_cls <- commas(paste0("{.cls ", cls, "}"))
    cli::cli_abort(
      paste0(
        "{.arg {arg}} must be a {len_msg} ", correct_cls, " vector, ",
        "not a {.cls {class(x)}}."
      ),
      arg = arg,
      call = error_call,
      class = "phinterval_error_wrong_class"
    )
  } else if (length(x) != n) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must have length {n}.",
        i = "{.arg {arg}} has length {length(x)}."
      ),
      arg = arg,
      call = error_call,
      class = "phinterval_error_wrong_class"
    )
  }

}

stop_not_list_of <- function(
    x,
    cls,
    arg = rlang::caller_arg(x),
    error_call = rlang::caller_env()
  ) {

  if (!is.list(x)) {
    cli::cli_abort(
      paste0(
        "{.arg {arg}} must be a {.cls list} of {.cls {cls}} vectors, ",
        "not a {.cls {class(x)}}."
      ),
      arg = arg,
      call = error_call,
      class = "phinterval_error_wrong_list_of"
    )
    return(invisible())
  }

  non_cls <- !map_lgl(x, inherits, cls)
  if (any(non_cls)) {
    ind <- which(non_cls)
    error_msg <- if (length(ind) == 1) {
      "Element at index {ind} is not a {.cls {cls}} vector."
    } else {
      "Elements at indices {ind} are not {.cls {cls}} vectors."
    }
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a list of {.cls {cls}} vectors.",
        x = error_msg
      ),
      arg = arg,
      call = error_call,
      class = "phinterval_error_wrong_list_of"
    )
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
    call = error_call,
    class = "phinterval_error_wrong_class"
  )

}
