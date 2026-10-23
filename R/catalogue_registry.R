#' Read catalogue registration
#'
#' Load the YAML catalogue registry used to configure connectors.
#'
#' @param path Path to a YAML file. Defaults to the bundled example config.
#'
#' @return A tibble with one row per configured catalogue.
#' @export
catalogue_registry <- function(
  path = system.file("config/catalogues.yml", package = "harvest")
) {
  if (is_empty_string(path) || !file.exists(path)) {
    return(tibble::tibble())
  }

  config <- yaml::read_yaml(path)
  catalogues <- config$catalogues %||% list()

  if (!length(catalogues)) {
    return(tibble::tibble())
  }

  purrr::map_dfr(catalogues, function(x) {
    tibble::tibble(
      id = x$id %||% x$name,
      type = x$type %||% NA_character_,
      name = x$name %||% x$id,
      base_url = x$base_url %||% NA_character_,
      api_base_url = x$api_base_url %||% NA_character_,
      enabled = isTRUE(x$enabled %||% TRUE),
      record_type = x$record_type %||% "dataset",
      discovery_path = x$discovery_path %||% NA_character_,
      api_path = x$api_path %||% NA_character_,
      query = x$query %||% NA_character_,
      parameters = list(x$parameters %||% list())
    )
  })
}

#' Create connector objects from a registry
#'
#' @param registry A tibble or data frame returned by the catalogue registry
#'   reader.
#' @param http_client Optional injected HTTP client settings stored in the
#'   connector configuration.
#'
#' @return A named list of connector objects.
#' @export
create_connectors <- function(registry, http_client = NULL) {
  if (!nrow(registry)) {
    return(list())
  }

  registry <- dplyr::as_tibble(registry) |>
    dplyr::filter(.data$enabled %in% TRUE)

  purrr::imap(
    split(registry, seq_len(nrow(registry))),
    function(row, idx) {
      row <- row[1, ]
      config <- list(
        record_type = row$record_type[[1]],
        discovery_path = row$discovery_path[[1]],
        api_path = row$api_path[[1]],
        api_base_url = row$api_base_url[[1]],
        query = row$query[[1]],
        parameters = row$parameters[[1]],
        http_client = http_client
      )

      create_connector_from_row(row, config = config)
    }
  ) |>
    rlang::set_names(registry$name)
}

#' @keywords internal
create_connector_from_row <- function(row, config = list()) {
  type <- row$type[[1]]
  name <- row$name[[1]]
  base_url <- row$base_url[[1]]

  switch(
    type,
    ckan = ckan_connector(
      base_url = base_url,
      name = name,
      config = config
    ),
    rds = rds_connector(
      base_url = base_url,
      name = name,
      config = config
    ),
    geonetwork = geonetwork_connector(
      base_url = base_url,
      name = name,
      config = config
    ),
    arcgis = arcgis_connector(
      base_url = base_url,
      name = name,
      config = config
    ),
    sparql = sparql_connector(
      base_url = base_url,
      name = name,
      config = config
    ),
    find = find_connector(
      base_url = base_url,
      name = name,
      config = config
    ),
    stop(sprintf("Unsupported connector type: %s", type))
  )
}
