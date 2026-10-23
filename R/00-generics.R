#' Connector generics
#'
#' Standard S3 generics used by all connector implementations.
#'
#' @name connector-generics
NULL

#' @export
discover <- function(x, ...) {
  UseMethod("discover")
}

#' @export
get_metadata <- function(x, ...) {
  UseMethod("get_metadata")
}

#' @export
normalise <- function(x, ...) {
  UseMethod("normalise")
}

#' @export
validate <- function(x, ...) {
  UseMethod("validate")
}
