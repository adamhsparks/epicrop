.onLoad <- function(libname, pkgname) {
  op <- options()
  defaults <- list(
    epicrop.use_rcpp = FALSE
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) {
    options(defaults[toset])
  }
  invisible()
}
