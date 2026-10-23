#' @keywords internal
html_read <- function(content) {
  xml2::read_html(content)
}

#' @keywords internal
html_meta_content <- function(doc, name) {
  node <- xml2::xml_find_first(
    doc,
    sprintf(".//meta[@name='%s' or @property='%s']", name, name)
  )
  val <- xml2::xml_attr(node, "content")
  if (is.na(val) || val == "") NA_character_ else val
}

#' @keywords internal
html_extract_links <- function(doc, pattern = NULL) {
  nodes <- xml2::xml_find_all(doc, ".//a[@href]")
  hrefs <- xml2::xml_attr(nodes, "href")
  texts <- xml2::xml_text(nodes, trim = TRUE)
  out <- tibble::tibble(href = hrefs, text = texts)
  if (!is.null(pattern)) {
    out <- dplyr::filter(out, grepl(pattern, .data$href))
  }
  out
}

#' @keywords internal
html_extract_labeled_pairs <- function(doc) {
  rows <- xml2::xml_find_all(doc, ".//table//tr")
  if (!length(rows)) {
    return(tibble::tibble(label = character(), value = character()))
  }

  purrr::map_dfr(rows, function(row) {
    cells <- xml2::xml_find_all(row, ".//th|.//td")
    if (length(cells) < 2) {
      return(tibble::tibble())
    }

    tibble::tibble(
      label = xml2::xml_text(cells[[1]], trim = TRUE),
      value = xml2::xml_text(cells[[2]], trim = TRUE)
    )
  })
}

#' @keywords internal
html_extract_tables <- function(doc) {
  tables <- xml2::xml_find_all(doc, ".//table")
  if (!length(tables)) {
    return(list())
  }

  purrr::map(tables, function(table_node) {
    rows <- xml2::xml_find_all(table_node, ".//tr")
    if (!length(rows)) {
      return(tibble::tibble())
    }

    cells <- purrr::map(rows, ~ xml2::xml_find_all(.x, ".//th|.//td"))
    widths <- purrr::map_int(cells, length)
    max_width <- max(widths)

    data <- purrr::map_dfr(seq_along(rows), function(i) {
      row <- rows[[i]]
      cols <- xml2::xml_find_all(row, ".//th|.//td")
      vals <- rep(NA_character_, max_width)
      vals[seq_along(cols)] <- xml2::xml_text(cols, trim = TRUE)
      tibble::as_tibble_row(stats::setNames(
        as.list(vals),
        paste0("V", seq_len(max_width))
      ))
    })

    header <- as.character(unlist(data[1, ], use.names = FALSE))
    if (all(!is.na(header)) && length(unique(header)) == ncol(data)) {
      names(data) <- make.unique(header)
      data <- data[-1, , drop = FALSE]
    }

    data
  })
}
