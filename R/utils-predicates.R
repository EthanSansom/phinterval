is_list_of_dbl <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, rlang::is_double))
}

is_list_of_POSIXct <- function(x) {
  rlang::is_list(x) && all(map_lgl(x, rlang::is_double))
}
