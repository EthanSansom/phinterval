#' Package options
#'
#' The `phinterval` package uses the following global options to control
#' printing and default behaviors. These options can be set using [options()]
#' and queried using [getOption()].
#'
#' @usage NULL
#' @format NULL
#'
#' @examples
#' monday <- phinterval(as.Date("2025-11-10"), as.Date("2025-11-11"))
#' friday <- phinterval(as.Date("2025-11-14"), as.Date("2025-11-15"))
#'
#' # Get the default setting
#' getOption("phinterval.print_max_width")
#' phint_squash(c(monday, friday))
#'
#' # Change the setting for the session duration
#' opts <- options(phinterval.print_max_width = 25)
#' phint_squash(c(monday, friday))
#'
#' # Reset to the previous settings
#' options(opts)
#'
#' @section Options:
phinterval_options <- list2(
  #' - `phinterval.print_max_width`: Character width at which a printed or
  #'    formatted `<phinterval>` element is truncated for display, default: `90`.
  phinterval.print_max_width = 90
)
