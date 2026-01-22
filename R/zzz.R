.onLoad <- function(libname, pkgname) {
  tzdb::tzdb_initialize()

  opts <- options()
  to_set <- !(names(phinterval_options) %in% names(opts))
  if (any(to_set)) options(phinterval_options[to_set])
  invisible()
}
