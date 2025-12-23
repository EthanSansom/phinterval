.onLoad <- function(libname, pkgname) {
  opts <- options()
  to_set <- !(names(phinterval_options) %in% names(opts))
  if (any(to_set)) options(phinterval_options[to_set])
  invisible()
}
