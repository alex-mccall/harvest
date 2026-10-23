#' Create a SPARQL connector
#'
#' @param base_url SPARQL endpoint URL.
#' @param name Catalogue name.
#' @param config Optional configuration list.
#'
#' @return A connector object.
#' @export
sparql_connector <- function(
  base_url,
  name = "Statistics.gov.scot",
  config = list()
) {
  new_connector("sparql", name = name, base_url = base_url, config = config)
}

#' @keywords internal
sparql_query_endpoint <- function(x) {
  endpoint <- connector_config(x, "api_base_url", x$base_url)
  endpoint <- sub("/$", "", endpoint)

  if (grepl("\\.json$", endpoint, ignore.case = TRUE)) {
    return(endpoint)
  }

  if (grepl("/sparql$", endpoint, ignore.case = TRUE)) {
    return(paste0(endpoint, ".json"))
  }

  paste0(endpoint, "/sparql.json")
}

#' @keywords internal
sparql_query_json <- function(x, query) {
  request_json_post(
    sparql_query_endpoint(x),
    body = list(query = query),
    headers = list(Accept = "application/sparql-results+json")
  )
}

#' @keywords internal
sparql_bindings_to_tibble <- function(bindings) {
  if (!length(bindings)) {
    return(tibble::tibble())
  }

  purrr::map_dfr(bindings, function(binding) {
    out <- purrr::imap(binding, function(value, name) {
      value$value %||% NA_character_
    })
    tibble::as_tibble_row(out)
  })
}

#' @keywords internal
record_value <- function(record, name, default = NULL) {
  if (!name %in% names(record)) {
    return(default)
  }

  value <- record[[name]]
  if (is.null(value) || (length(value) == 1 && is.na(value))) {
    return(default)
  }

  value
}

#' @export
discover.sparql_connector <- function(x, limit = 200, ...) {
  query <- paste(
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "PREFIX dct: <http://purl.org/dc/terms/>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "SELECT DISTINCT ?dataset ?label ?description ?publisher ?modified",
    "WHERE {",
    "  ?dataset a qb:DataSet .",
    "  OPTIONAL { ?dataset rdfs:label ?label . }",
    "  OPTIONAL { ?dataset dct:description ?description . }",
    "  OPTIONAL { ?dataset dct:publisher ?publisher . }",
    "  OPTIONAL { ?dataset dct:modified ?modified . }",
    "  FILTER(STRSTARTS(STR(?dataset), \"http://statistics.gov.scot/data/\"))",
    "}",
    sprintf("LIMIT %s", limit),
    sep = "\n"
  )

  result <- tryCatch(
    sparql_query_json(x, query),
    error = function(e) {
      log_warn(
        "%s discovery failed: %s",
        connector_label(x),
        conditionMessage(e)
      )
      list(results = list(bindings = list()))
    }
  )

  bindings <- result$results$bindings %||% list()
  tbl <- sparql_bindings_to_tibble(bindings)
  if (!nrow(tbl) || !"dataset" %in% names(tbl)) {
    return(tibble::tibble(
      record_type = character(),
      dataset_id = character(),
      dataset_name = character(),
      source_catalogue = character(),
      source_url = character()
    ))
  }

  tbl |>
    dplyr::transmute(
      record_type = "dataset",
      dataset_id = dplyr::if_else(
        !is.na(.data$dataset),
        sub("^.*/", "", .data$dataset),
        NA_character_
      ),
      dataset_name = dplyr::coalesce(.data$label, .data$dataset_id),
      source_catalogue = x$name,
      source_url = .data$dataset
    )
}

#' @keywords internal
sparql_describe_dataset <- function(x, dataset_uri) {
  query <- paste(
    "PREFIX qb: <http://purl.org/linked-data/cube#>",
    "PREFIX dct: <http://purl.org/dc/terms/>",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>",
    "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>",
    "SELECT DISTINCT ?label ?description ?publisher ?modified ?theme ?spatial ?temporal ?license",
    "WHERE {",
    sprintf("  BIND(<%s> AS ?dataset)", dataset_uri),
    "  OPTIONAL { ?dataset rdfs:label ?label . }",
    "  OPTIONAL { ?dataset dct:description ?description . }",
    "  OPTIONAL { ?dataset dct:publisher ?publisher . }",
    "  OPTIONAL { ?dataset dct:modified ?modified . }",
    "  OPTIONAL { ?dataset dct:subject ?theme . }",
    "  OPTIONAL { ?dataset dct:spatial ?spatial . }",
    "  OPTIONAL { ?dataset dct:temporal ?temporal . }",
    "  OPTIONAL { ?dataset dct:license ?license . }",
    "}",
    sep = "\n"
  )

  result <- sparql_query_json(x, query)
  sparql_bindings_to_tibble(result$results$bindings %||% list())
}

#' @export
get_metadata.sparql_connector <- function(x, data, ...) {
  uris <- if (is.data.frame(data)) data$source_url else as.character(data)
  uris <- unique(uris)

  purrr::map(uris, function(uri) {
    tryCatch(
      sparql_describe_dataset(x, uri),
      error = function(e) {
        log_warn(
          "%s dataset fetch failed for %s: %s",
          connector_label(x),
          uri,
          conditionMessage(e)
        )
        tibble::tibble()
      }
    )
  })
}

#' @export
normalise.sparql_connector <- function(x, data, ...) {
  if (!length(data)) {
    return(canonical_metadata_template())
  }

  purrr::map_dfr(data, function(record) {
    if (!is.data.frame(record) || !nrow(record)) {
      return(canonical_metadata_template())
    }

    title <- record_value(record, "label", NA_character_) %||%
      record_value(record, "dataset", NA_character_) %||%
      NA_character_
    description <- record_value(record, "description", NA_character_)
    publisher <- record_value(record, "publisher", NA_character_)
    dataset_uri <- record_value(record, "dataset", NA_character_) %||%
      record_value(record, "source_url", NA_character_) %||%
      NA_character_
    theme <- record_value(record, "theme", character())
    spatial <- record_value(record, "spatial", character())
    temporal <- record_value(record, "temporal", character())
    license <- record_value(record, "license", NA_character_)
    modified <- record_value(record, "modified", NA_character_)

    if (is_empty_string(dataset_uri)) {
      return(canonical_metadata_template())
    }

    canonical_metadata_row(
      record_type = "dataset",
      dataset_id = sub("^.*/", "", dataset_uri),
      dataset_name = title %||% dataset_uri,
      description = description,
      publisher = publisher,
      source_catalogue = x$name,
      source_url = dataset_uri,
      organisation = publisher,
      keywords = c(theme %||% character()),
      themes = c(theme %||% character()),
      geographic_coverage = c(spatial %||% character()),
      temporal_coverage = c(temporal %||% character()),
      access_type = NA_character_,
      access_constraints = NA_character_,
      licence = license %||% NA_character_,
      update_frequency = NA_character_,
      metadata_last_updated = modified %||% NA_character_,
      harvest_timestamp = Sys.time(),
      variables = list(record),
      lineage = list(list(
        sparql = record
      ))
    )
  }) |>
    validate_canonical_metadata()
}

#' @export
validate.sparql_connector <- function(x, data, ...) {
  validate_canonical_metadata(data)
}
