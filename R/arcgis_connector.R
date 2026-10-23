#' Create an ArcGIS connector
#'
#' @param base_url Base URL of the ArcGIS Hub or ArcGIS REST endpoint.
#' @param name Catalogue name.
#' @param config Optional configuration list.
#'
#' @return A connector object.
#' @export
arcgis_connector <- function(
  base_url,
  name = "NatureScot Spatial Hub",
  config = list()
) {
  new_connector("arcgis", name = name, base_url = base_url, config = config)
}

#' @keywords internal
arcgis_search_endpoints <- function(x) {
  c(
    connector_config(x, "api_path", "/api/search/v1"),
    "/sharing/rest/search",
    "/api/search"
  )
}

#' @keywords internal
arcgis_search_items <- function(x, q = "*", num = 100) {
  for (path in arcgis_search_endpoints(x)) {
    url <- connector_source_url(x, path)
    result <- tryCatch(
      request_json(url, query = list(f = "json", q = q, num = num)),
      error = function(e) NULL
    )

    if (!is.null(result)) {
      items <- result$results %||% result$items %||% result$data %||% list()
      if (length(items)) {
        return(items)
      }
    }
  }

  list()
}

#' @export
discover.arcgis_connector <- function(x, q = "*", num = 100, ...) {
  items <- tryCatch(
    arcgis_search_items(x, q = q, num = num),
    error = function(e) {
      log_warn(
        "%s discovery failed: %s",
        connector_label(x),
        conditionMessage(e)
      )
      list()
    }
  )

  if (!length(items)) {
    return(tibble::tibble(
      record_type = character(),
      dataset_id = character(),
      dataset_name = character(),
      source_catalogue = character(),
      source_url = character(),
      item_type = character()
    ))
  }

  purrr::map_dfr(items, function(item) {
    item_id <- item$id %||%
      item$itemId %||%
      item$serviceItemId %||%
      NA_character_
    title <- item$title %||% item$name %||% NA_character_
    type <- item$type %||% item$typeKeywords %||% NA_character_
    tibble::tibble(
      record_type = "dataset",
      dataset_id = item_id,
      dataset_name = title,
      source_catalogue = x$name,
      source_url = item$url %||%
        if (!is.na(item_id)) {
          connector_source_url(
            x,
            paste0("/sharing/rest/content/items/", item_id)
          )
        } else {
          NA_character_
        },
      item_type = type
    )
  }) |>
    dplyr::filter(!is.na(.data$dataset_id))
}

#' @keywords internal
arcgis_item_details <- function(x, id) {
  paths <- c(
    paste0("/sharing/rest/content/items/", id),
    paste0("/api/content/items/", id)
  )

  for (path in paths) {
    url <- connector_source_url(x, path)
    result <- tryCatch(
      request_json(url, query = list(f = "json")),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      return(result)
    }
  }

  list(id = id)
}

#' @keywords internal
arcgis_variables_from_service <- function(x, item) {
  service_url <- item$url %||%
    item$serviceurl %||%
    item$serviceUrl %||%
    item$serviceUrl
  if (is_empty_string(service_url)) {
    return(list())
  }

  details <- tryCatch(
    request_json(service_url, query = list(f = "json")),
    error = function(e) list()
  )

  layers <- details$layers %||% details$tables %||% list()
  if (!length(layers)) {
    return(list())
  }

  purrr::map(layers, function(layer) {
    fields <- layer$fields %||% list()
    if (!length(fields)) {
      return(tibble::tibble())
    }

    tibble::tibble(
      layer_id = layer$id %||% NA_integer_,
      layer_name = layer$name %||% NA_character_,
      field_name = vapply(
        fields,
        function(field) field$name %||% NA_character_,
        character(1)
      ),
      field_alias = vapply(
        fields,
        function(field) field$alias %||% NA_character_,
        character(1)
      ),
      field_type = vapply(
        fields,
        function(field) field$type %||% NA_character_,
        character(1)
      )
    )
  })
}

#' @export
get_metadata.arcgis_connector <- function(x, data, ...) {
  ids <- if (is.data.frame(data)) data$dataset_id else as.character(data)
  ids <- unique(ids)

  purrr::map(ids, function(id) {
    tryCatch(
      arcgis_item_details(x, id),
      error = function(e) {
        log_warn(
          "%s item fetch failed for %s: %s",
          connector_label(x),
          id,
          conditionMessage(e)
        )
        list(id = id)
      }
    )
  })
}

#' @export
normalise.arcgis_connector <- function(x, data, ...) {
  if (!length(data)) {
    return(canonical_metadata_template())
  }

  purrr::map_dfr(data, function(item) {
    id <- item$id %||% item$itemId %||% item$serviceItemId %||% NA_character_
    title <- item$title %||% item$name %||% id
    description <- item$description %||% item$snippet %||% NA_character_
    organisation <- item$owner %||%
      item$orgName %||%
      item$publisher %||%
      NA_character_
    update_time <- item$modified %||% item$lastModified %||% NA_character_

    canonical_metadata_row(
      record_type = "dataset",
      dataset_id = id,
      dataset_name = title,
      description = description,
      publisher = organisation,
      source_catalogue = x$name,
      source_url = item$url %||%
        item$homePageUrl %||%
        connector_source_url(x, paste0("/sharing/rest/content/items/", id)),
      organisation = organisation,
      keywords = item$tags %||% character(),
      themes = item$categories %||% character(),
      geographic_coverage = c(
        item$extent %||% character(),
        item$spatialReference %||% character()
      ),
      temporal_coverage = c(
        item$startDate %||% character(),
        item$endDate %||% character()
      ),
      access_type = item$accessInformation %||%
        item$accessType %||%
        NA_character_,
      access_constraints = item$accessConstraints %||% NA_character_,
      licence = item$licenseInfo %||% item$license %||% NA_character_,
      update_frequency = item$updateFrequency %||% NA_character_,
      metadata_last_updated = update_time,
      harvest_timestamp = Sys.time(),
      variables = list(arcgis_variables_from_service(x, item)),
      lineage = list(list(
        raw_item = item,
        service = item$url %||% item$serviceUrl %||% NA_character_
      ))
    )
  }) |>
    validate_canonical_metadata()
}

#' @export
validate.arcgis_connector <- function(x, data, ...) {
  validate_canonical_metadata(data)
}
