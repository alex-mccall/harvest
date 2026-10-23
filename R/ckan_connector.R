#' Create a CKAN connector
#'
#' @param base_url Base URL of the CKAN instance.
#' @param api_base_url Optional API base URL if the catalogue API is hosted on a
#'   different domain.
#' @param name Human-readable catalogue name.
#' @param config Optional configuration list.
#'
#' @return A CKAN connector object.
#' @export
ckan_connector <- function(
    base_url,
    name = "CKAN catalogue",
    api_base_url = NULL,
    config = list()
) {
  if (!is.null(api_base_url)) {
    config$api_base_url <- api_base_url
  }
  new_connector("ckan", name = name, base_url = base_url, config = config)
}

#' CKAN package list
#'
#' @keywords internal
ckan_package_list <- function(x) {
  body <- request_json(
    connector_api_url(x, "/api/3/action/package_list"),
    query = list()
  )

  body$result %||% character()
}

#' CKAN package search
#'
#' @keywords internal
ckan_package_search <- function(x, query = "", rows = 100, start = 0) {
  body <- request_json(
    connector_api_url(x, "/api/3/action/package_search"),
    query = list(q = query, rows = rows, start = start)
  )

  body$result$results %||% list()
}

#' CKAN package show
#'
#' @keywords internal
ckan_package_show <- function(x, id) {
  body <- request_json(
    connector_api_url(x, "/api/3/action/package_show"),
    query = list(id = id)
  )

  body$result %||% list()
}

#' CKAN resource show
#'
#' @keywords internal
ckan_resource_show <- function(x, id) {
  body <- request_json(
    connector_api_url(x, "/api/3/action/resource_show"),
    query = list(id = id)
  )

  body$result %||% list()
}

#' @export
discover.ckan_connector <- function(x, ...) {
  ids <- tryCatch(
    ckan_package_list(x),
    error = function(e) {
      log_warn(
        "%s package_list failed: %s",
        connector_label(x),
        conditionMessage(e)
      )
      character()
    }
  )

  tibble::tibble(
    dataset_id = ids,
    record_type = connector_record_type(x),
    source_catalogue = x$name,
    source_url = vapply(
      ids,
      function(id) connector_source_url(x, paste0("/dataset/", id)),
      character(1)
    )
  )
}

#' @export
get_metadata.ckan_connector <- function(x, data, ...) {
  ids <- if (is.data.frame(data)) data$dataset_id else as.character(data)
  ids <- unique(ids)

  log_info(
    "Harvesting %s CKAN records from %s",
    length(ids),
    connector_label(x)
  )

  purrr::map(ids, function(id) {
    tryCatch(
      ckan_package_show(x, id),
      error = function(e) {
        log_warn(
          "%s package_show failed for %s: %s",
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
normalise.ckan_connector <- function(x, data, ...) {
  if (!length(data)) {
    return(canonical_metadata_template())
  }

  purrr::map_dfr(data, function(pkg) {
    if (is.null(pkg$id) && is.null(pkg$name)) {
      return(canonical_metadata_template())
    }

    resources <- pkg$resources %||% list()
    resource_tbl <- if (length(resources)) {
      tibble::tibble(
        id = purrr::map_chr(resources, function(r) r$id %||% NA_character_),
        name = purrr::map_chr(resources, function(r) r$name %||% NA_character_),
        format = purrr::map_chr(resources, function(r) {
          r$format %||% NA_character_
        }),
        url = purrr::map_chr(resources, function(r) r$url %||% NA_character_),
        resource_type = purrr::map_chr(resources, function(r) {
          r$resource_type %||% NA_character_
        }),
        last_modified = purrr::map_chr(resources, function(r) {
          r$last_modified %||% NA_character_
        })
      )
    } else {
      tibble::tibble()
    }

    canonical_metadata_row(
      record_type = "dataset",
      dataset_id = pkg$id %||% pkg$name,
      dataset_name = pkg$title %||% pkg$name %||% pkg$id,
      description = pkg$notes %||% pkg$description %||% NA_character_,
      publisher = pkg$organization$title %||%
        pkg$organization$name %||%
        NA_character_,
      source_catalogue = x$name,
      source_url = connector_source_url(
        x,
        paste0("/dataset/", pkg$name %||% pkg$id)
      ),
      organisation = pkg$organization$title %||%
        pkg$organization$name %||%
        NA_character_,
      keywords = vapply(
        pkg$tags %||% list(),
        function(tag) tag$name %||% "",
        character(1)
      ),
      themes = vapply(
        pkg$groups %||% list(),
        function(group) group$title %||% group$name %||% "",
        character(1)
      ),
      geographic_coverage = c(
        pkg$spatial_coverage %||% character(),
        pkg$region %||% character()
      ),
      temporal_coverage = c(
        pkg$temporal_coverage_start %||% character(),
        pkg$temporal_coverage_end %||% character()
      ),
      access_type = pkg$access_type %||% pkg$private %||% NA_character_,
      access_constraints = pkg$access_constraints %||% NA_character_,
      licence = pkg$license_title %||% pkg$license_id %||% NA_character_,
      update_frequency = pkg$frequency %||%
        pkg$frequency_update %||%
        pkg$update_frequency %||%
        NA_character_,
      metadata_last_updated = pkg$metadata_modified %||%
        pkg$metadata_created %||%
        NA_character_,
      harvest_timestamp = Sys.time(),
      variables = list(),
      lineage = list(list(
        package = pkg,
        resources = resource_tbl
      ))
    )
  }) |>
    validate_canonical_metadata()
}

#' @export
validate.ckan_connector <- function(x, data, ...) {
  validate_canonical_metadata(data)
}
