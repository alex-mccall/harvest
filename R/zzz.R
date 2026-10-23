onLoad <- function(libname, pkgname) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_threshold(logger::INFO)
  }
}
