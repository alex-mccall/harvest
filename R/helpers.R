`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

is_empty_string <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }

  x <- as.character(x)
  !length(x) || all(is.na(x) | trimws(x) == "")
}

normalize_text <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

collapse_text <- function(x, sep = " | ") {
  x <- normalize_text(x)
  x <- x[!is.na(x)]

  if (!length(x)) {
    return(NA_character_)
  }

  paste(unique(x), collapse = sep)
}

as_list_value <- function(x) {
  if (is.null(x)) {
    return(list())
  }

  if (length(x) == 1 && is.list(x)) {
    return(x)
  }

  list(x)
}

to_character_vector <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }

  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  unique(x)
}

jsonify_value <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")
}

safe_url_join <- function(base_url, path) {
  base_url <- sub("/$", "", base_url)
  path <- sub("^/", "", path)
  paste0(base_url, "/", path)
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  invisible(path)
}

tokenise <- function(x) {
  x <- collapse_text(x, sep = " ")
  if (is.na(x)) {
    return(character())
  }

  tokens <- unlist(
    strsplit(tolower(x), "[^[:alnum:]]+", perl = TRUE),
    use.names = FALSE
  )
  tokens <- tokens[tokens != ""]
  unique(tokens)
}

textify_list_column <- function(x) {
  if (is.list(x)) {
    return(vapply(x, collapse_text, character(1), sep = " ; "))
  }

  as.character(x)
}

log_info <- function(...) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_info(...)
  } else {
    message(sprintf(...))
  }
}

log_warn <- function(...) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_warn(...)
  } else {
    warning(sprintf(...), call. = FALSE)
  }
}

log_debug <- function(...) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_debug(...)
  } else {
    invisible(NULL)
  }
}

log_error <- function(...) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_error(...)
  } else {
    stop(sprintf(...), call. = FALSE)
  }
}
