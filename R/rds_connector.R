#' Create a Research Data Scotland connector
#'
#' @param base_url Base URL of the metadata catalogue.
#' @param name Catalogue name.
#' @param config Optional configuration list.
#'
#' @return A connector object.
#' @export
rds_connector <- function(
  base_url,
  name = "Research Data Scotland Metadata Catalogue",
  config = list()
) {
  new_connector("rds", name = name, base_url = base_url, config = config)
}

#' @keywords internal
rds_catalogue_links <- function(x) {
  paths <- c(
    "/metadata-catalogue/",
    "/metadata-catalogue/search-results/",
    "/metadata-catalogue/search-results/result/"
  )

  purrr::map(paths, function(path) {
    url <- connector_source_url(x, path)
    tryCatch(
      request_text(url),
      error = function(e) ""
    )
  })
}

#' @keywords internal
rds_discover_links <- function(x) {
  htmls <- rds_catalogue_links(x)
  htmls <- purrr::keep(htmls, ~ !is_empty_string(.x))
  if (!length(htmls)) {
    return(tibble::tibble(href = character(), text = character()))
  }

  links <- purrr::map_dfr(htmls, function(html) {
    doc <- html_read(html)
    html_extract_links(doc)
  })

  links |>
    dplyr::filter(stringr::str_detect(
      .data$href,
      "search-results/result|metadata-catalogue"
    )) |>
    dplyr::mutate(
      href = xml2::url_absolute(.data$href, x$base_url)
    ) |>
    dplyr::distinct(.data$href, .keep_all = TRUE)
}

#' @export
discover.rds_connector <- function(x, entity = "dataset", ...) {
  links <- tryCatch(
    rds_discover_links(x),
    error = function(e) {
      log_warn(
        "%s discovery failed: %s",
        connector_label(x),
        conditionMessage(e)
      )
      tibble::tibble(href = character(), text = character())
    }
  )

  tibble::tibble(
    record_type = if (entity %in% c("organisation", "owner")) {
      entity
    } else {
      "dataset"
    },
    dataset_id = sub(".*/", "", links$href),
    dataset_name = links$text,
    source_catalogue = x$name,
    source_url = links$href
  )
}

#' @keywords internal
rds_parse_variables <- function(doc) {
  tables <- html_extract_tables(doc)
  if (!length(tables)) {
    return(list())
  }

  variable_tables <- purrr::keep(tables, ~ ncol(.x) >= 2)
  if (!length(variable_tables)) {
    return(list())
  }

  purrr::map(variable_tables, function(tbl) {
    names(tbl) <- make.names(names(tbl), unique = TRUE)
    tbl
  })
}

#' @keywords internal
rds_safe_haven_links <- function(doc) {
  links <- html_extract_links(doc)
  providers <- c(
    "National Safe Haven",
    "DaSH",
    "DataLoch",
    "HIC",
    "West of Scotland Safe Haven"
  )

  links |>
    dplyr::filter(purrr::map_lgl(.data$text, function(txt) {
      any(stringr::str_detect(txt, providers))
    }))
}

#' @keywords internal
rds_page_metadata <- function(x, url) {
  html <- request_text(url)
  doc <- html_read(html)
  pairs <- html_extract_labeled_pairs(doc)
  title <- html_meta_content(doc, "og:title") %||%
    html_meta_content(doc, "title")
  description <- html_meta_content(doc, "description")
  variables <- rds_parse_variables(doc)
  safe_haven <- html_extract_links(doc) |>
    dplyr::filter(stringr::str_detect(
      tolower(.data$text),
      "safe haven|dash|dataloch|hic"
    ))

  list(
    url = url,
    title = title,
    description = description,
    pairs = pairs,
    variables = variables,
    safe_haven = safe_haven,
    html = html
  )
}

#' @export
get_metadata.rds_connector <- function(x, data, ...) {
  urls <- if (is.data.frame(data)) data$source_url else as.character(data)
  urls <- unique(urls)

  purrr::map(urls, function(url) {
    tryCatch(
      rds_page_metadata(x, url),
      error = function(e) {
        log_warn(
          "%s page fetch failed for %s: %s",
          connector_label(x),
          url,
          conditionMessage(e)
        )
        list(url = url)
      }
    )
  })
}

#' @export
normalise.rds_connector <- function(x, data, ...) {
  if (!length(data)) {
    return(canonical_metadata_template())
  }

  purrr::map_dfr(data, function(page) {
    pairs <- page$pairs %||%
      tibble::tibble(label = character(), value = character())
    title <- page$title %||% NA_character_
    description <- page$description %||% NA_character_

    organisation <- pairs |>
      dplyr::filter(stringr::str_detect(
        tolower(.data$label),
        "organisation|provider|host|owner"
      )) |>
      dplyr::pull(.data$value) |>
      collapse_text()

    provider_links <- page$safe_haven %||%
      tibble::tibble(href = character(), text = character())
    safe_haven_provider <- provider_links$text |> collapse_text()

    variable_payload <- page$variables %||% list()

    canonical_metadata_row(
      record_type = connector_record_type(x),
      dataset_id = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "id|identifier"
        )) |>
        dplyr::pull(.data$value) |>
        collapse_text() %||%
        sub(".*/", "", page$url),
      dataset_name = title %||% sub(".*/", "", page$url),
      description = description,
      publisher = organisation,
      source_catalogue = x$name,
      source_url = page$url,
      organisation = organisation,
      keywords = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "topic|keyword|theme"
        )) |>
        dplyr::pull(.data$value),
      themes = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "topic|theme"
        )) |>
        dplyr::pull(.data$value),
      geographic_coverage = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "geographic|coverage|location"
        )) |>
        dplyr::pull(.data$value),
      temporal_coverage = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "temporal|period|date"
        )) |>
        dplyr::pull(.data$value),
      access_type = pairs |>
        dplyr::filter(stringr::str_detect(tolower(.data$label), "access")) |>
        dplyr::pull(.data$value) |>
        collapse_text(),
      access_constraints = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "constraint|condition|usage"
        )) |>
        dplyr::pull(.data$value) |>
        collapse_text(),
      licence = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "licence|license"
        )) |>
        dplyr::pull(.data$value) |>
        collapse_text(),
      update_frequency = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "update|frequency|refresh"
        )) |>
        dplyr::pull(.data$value) |>
        collapse_text(),
      metadata_last_updated = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "last updated|metadata last updated|updated"
        )) |>
        dplyr::pull(.data$value) |>
        collapse_text(),
      harvest_timestamp = Sys.time(),
      variables = list(variable_payload),
      lineage = list(list(
        safe_haven_provider = safe_haven_provider,
        safe_haven_links = provider_links,
        raw_pairs = pairs
      ))
    )
  }) |>
    validate_canonical_metadata()
}

#' @export
validate.rds_connector <- function(x, data, ...) {
  validate_canonical_metadata(data)
}
