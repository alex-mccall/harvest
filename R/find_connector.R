#' Create a Find.Data.Gov.Scot connector
#'
#' This connector currently uses public HTML pages and sitemap links because
#' the service presents itself as a search portal with metadata quality and
#' organisation pages, and a stable public API is not documented in the
#' sources inspected.
#'
#' @param base_url Base URL for the portal.
#' @param name Catalogue name.
#' @param config Configuration list.
#'
#' @return A connector object.
#' @export
find_connector <- function(
  base_url,
  name = "Find.Data.Gov.Scot",
  config = list()
) {
  new_connector("find", name = name, base_url = base_url, config = config)
}

#' @keywords internal
find_sitemap_urls <- function(x) {
  paths <- c("/sitemap.xml", "/sitemap_index.xml", "/sitemap-index.xml")
  contents <- purrr::map(paths, function(path) {
    url <- connector_source_url(x, path)
    tryCatch(
      request_text(url),
      error = function(e) ""
    )
  })

  contents <- purrr::keep(contents, ~ !is_empty_string(.x))
  if (!length(contents)) {
    return(character())
  }

  purrr::map(contents, function(content) {
    tryCatch(
      xml2::read_xml(content),
      error = function(e) NULL
    )
  }) |>
    purrr::compact() |>
    purrr::map(xml2::xml_find_all, ".//loc") |>
    purrr::map(xml2::xml_text) |>
    unlist(use.names = FALSE) |>
    unique()
}

#' @keywords internal
find_extract_links <- function(x, entity = "dataset") {
  urls <- find_sitemap_urls(x)
  pattern <- switch(
    entity,
    dataset = "/datasets/",
    organisation = "/organisation/",
    owner = "/organisation/",
    "/datasets/"
  )

  urls[grepl(pattern, urls, fixed = TRUE)]
}

#' @export
discover.find_connector <- function(x, entity = "dataset", ...) {
  urls <- tryCatch(
    find_extract_links(x, entity = entity),
    error = function(e) {
      log_warn(
        "%s discovery failed: %s",
        connector_label(x),
        conditionMessage(e)
      )
      character()
    }
  )

  tibble::tibble(
    record_type = if (entity %in% c("organisation", "owner")) {
      entity
    } else {
      "dataset"
    },
    dataset_id = sub(".*/", "", urls),
    dataset_name = NA_character_,
    source_catalogue = x$name,
    source_url = urls
  )
}

#' @keywords internal
find_page_metadata <- function(x, url) {
  html <- request_text(url)
  doc <- html_read(html)
  pairs <- html_extract_labeled_pairs(doc)
  links <- html_extract_links(doc)

  list(
    url = url,
    title = html_meta_content(doc, "og:title") %||%
      xml2::xml_text(xml2::xml_find_first(doc, ".//title"), trim = TRUE),
    description = html_meta_content(doc, "description"),
    pairs = pairs,
    links = links,
    html = html
  )
}

#' @export
get_metadata.find_connector <- function(x, data, ...) {
  urls <- if (is.data.frame(data)) data$source_url else as.character(data)
  urls <- unique(urls)

  purrr::map(urls, function(url) {
    tryCatch(
      find_page_metadata(x, url),
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
normalise.find_connector <- function(x, data, ...) {
  if (!length(data)) {
    return(canonical_metadata_template())
  }

  purrr::map_dfr(data, function(page) {
    pairs <- page$pairs %||%
      tibble::tibble(label = character(), value = character())
    links <- page$links %||%
      tibble::tibble(href = character(), text = character())
    title <- page$title %||% NA_character_
    description <- page$description %||% NA_character_

    organisation <- pairs |>
      dplyr::filter(stringr::str_detect(
        tolower(.data$label),
        "host|owner|data provider"
      )) |>
      dplyr::pull(.data$value) |>
      collapse_text()

    licence <- pairs |>
      dplyr::filter(stringr::str_detect(
        tolower(.data$label),
        "licence|license|usage rights"
      )) |>
      dplyr::pull(.data$value) |>
      collapse_text()

    access_type <- pairs |>
      dplyr::filter(stringr::str_detect(tolower(.data$label), "access")) |>
      dplyr::pull(.data$value) |>
      collapse_text()

    update_frequency <- pairs |>
      dplyr::filter(stringr::str_detect(
        tolower(.data$label),
        "updated|last updated"
      )) |>
      dplyr::pull(.data$value) |>
      collapse_text()

    source_url <- page$url
    dataset_id <- sub(".*/", "", source_url)
    keywords <- pairs |>
      dplyr::filter(stringr::str_detect(
        tolower(.data$label),
        "topic|keyword|theme"
      )) |>
      dplyr::pull(.data$value)

    quality_score <- NA_real_
    quality_row <- pairs |>
      dplyr::filter(stringr::str_detect(
        tolower(.data$label),
        "metadata quality"
      )) |>
      dplyr::slice_head(n = 1)
    if (nrow(quality_row)) {
      score <- stringr::str_extract(quality_row$value[[1]], "[0-9]+")
      quality_score <- suppressWarnings(as.numeric(score))
    }

    canonical_metadata_row(
      record_type = connector_record_type(x),
      dataset_id = dataset_id,
      dataset_name = title %||% dataset_id,
      description = description %||% NA_character_,
      publisher = organisation,
      source_catalogue = x$name,
      source_url = source_url,
      organisation = organisation,
      keywords = keywords,
      themes = keywords,
      geographic_coverage = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "geographic|coverage|location"
        )) |>
        dplyr::pull(.data$value),
      access_type = access_type,
      access_constraints = NA_character_,
      licence = licence,
      update_frequency = update_frequency,
      metadata_last_updated = pairs |>
        dplyr::filter(stringr::str_detect(
          tolower(.data$label),
          "last updated|updated"
        )) |>
        dplyr::pull(.data$value) |>
        collapse_text(),
      harvest_timestamp = Sys.time(),
      variables = list(),
      lineage = list(list(
        links = links,
        raw_pairs = pairs,
        quality_score = quality_score
      )),
      quality_score = quality_score
    )
  }) |>
    validate_canonical_metadata()
}

#' @export
validate.find_connector <- function(x, data, ...) {
  validate_canonical_metadata(data)
}
