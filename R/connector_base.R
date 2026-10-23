#' Create a connector object
#'
#' @keywords internal
new_connector <- function(type, name, base_url, config = list()) {
  stopifnot(is.character(type), length(type) == 1)
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.character(base_url), length(base_url) == 1)

  structure(
    list(
      type = type,
      name = name,
      base_url = sub("/$", "", base_url),
      config = config
    ),
    class = c(paste0(type, "_connector"), "harvest_connector")
  )
}

#' @keywords internal
connector_label <- function(x) {
  paste0(x$name, " [", x$type, "]")
}

#' @keywords internal
connector_config <- function(x, name, default = NULL) {
  value <- x$config[[name]]
  if (is.null(value) || (length(value) == 1 && is.na(value))) {
    return(default)
  }

  value
}

#' @keywords internal
connector_record_type <- function(x) {
  connector_config(x, "record_type", "dataset")
}

#' @keywords internal
connector_api_base_url <- function(x) {
  connector_config(x, "api_base_url", x$base_url) %||% x$base_url
}

#' @keywords internal
connector_api_url <- function(x, path = "") {
  base_url <- connector_api_base_url(x)
  if (is_empty_string(path)) {
    return(base_url)
  }

  safe_url_join(base_url, path)
}

#' @keywords internal
connector_source_url <- function(x, path = "") {
  if (is_empty_string(path)) {
    return(x$base_url)
  }

  safe_url_join(x$base_url, path)
}

#' @keywords internal
request_json <- function(url, query = list(), headers = list(), timeout = 60) {
  req <- httr2::request(url) |>
    httr2::req_user_agent("harvest/0.1.0") |>
    httr2::req_timeout(timeout)

  if (length(query)) {
    req <- rlang::exec(httr2::req_url_query, req, !!!query)
  }

  if (length(headers)) {
    req <- rlang::exec(httr2::req_headers, req, !!!headers)
  }

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' @keywords internal
request_json_post <- function(url, body = list(), headers = list(), timeout = 60) {
  req <- httr2::request(url) |>
    httr2::req_user_agent("harvest/0.1.0") |>
    httr2::req_timeout(timeout) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body)

  if (length(headers)) {
    req <- rlang::exec(httr2::req_headers, req, !!!headers)
  }

  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' @keywords internal
request_text <- function(url, query = list(), headers = list(), timeout = 60) {
  req <- httr2::request(url) |>
    httr2::req_user_agent("harvest/0.1.0") |>
    httr2::req_timeout(timeout)

  if (length(query)) {
    req <- rlang::exec(httr2::req_url_query, req, !!!query)
  }

  if (length(headers)) {
    req <- rlang::exec(httr2::req_headers, req, !!!headers)
  }

  resp <- httr2::req_perform(req)
  httr2::resp_body_string(resp)
}

#' @keywords internal
read_json_safe <- function(text) {
  if (is_empty_string(text)) {
    return(list())
  }

  jsonlite::fromJSON(text, simplifyVector = FALSE)
}
