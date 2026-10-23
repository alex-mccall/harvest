#' Canonical metadata template
#'
#' Create an empty canonical metadata tibble with the expected columns.
#'
#' @return A zero-row tibble defining the canonical schema.
#' @export
canonical_metadata_template <- function() {
  tibble::tibble(
    record_type = character(),
    dataset_id = character(),
    dataset_name = character(),
    description = character(),
    publisher = character(),
    source_catalogue = character(),
    source_url = character(),
    organisation = character(),
    keywords = list(),
    themes = list(),
    geographic_coverage = list(),
    temporal_coverage = list(),
    access_type = character(),
    access_constraints = character(),
    licence = character(),
    update_frequency = character(),
    metadata_last_updated = as.POSIXct(character()),
    harvest_timestamp = as.POSIXct(character()),
    variables = list(),
    lineage = list(),
    quality_score = numeric()
  )
}

#' Build one canonical metadata row
#'
#' @keywords internal
canonical_metadata_row <- function(
  record_type = "dataset",
  dataset_id,
  dataset_name,
  description = NA_character_,
  publisher = NA_character_,
  source_catalogue,
  source_url = NA_character_,
  organisation = NA_character_,
  keywords = character(),
  themes = character(),
  geographic_coverage = character(),
  temporal_coverage = character(),
  access_type = NA_character_,
  access_constraints = NA_character_,
  licence = NA_character_,
  update_frequency = NA_character_,
  metadata_last_updated = as.POSIXct(NA),
  harvest_timestamp = Sys.time(),
  variables = list(),
  lineage = list(),
  quality_score = NA_real_
) {
  tibble::tibble(
    record_type = as.character(record_type),
    dataset_id = as.character(dataset_id),
    dataset_name = as.character(dataset_name),
    description = as.character(description),
    publisher = as.character(publisher),
    source_catalogue = as.character(source_catalogue),
    source_url = as.character(source_url),
    organisation = as.character(organisation),
    keywords = list(to_character_vector(keywords)),
    themes = list(to_character_vector(themes)),
    geographic_coverage = list(to_character_vector(geographic_coverage)),
    temporal_coverage = list(to_character_vector(temporal_coverage)),
    access_type = as.character(access_type),
    access_constraints = as.character(access_constraints),
    licence = as.character(licence),
    update_frequency = as.character(update_frequency),
    metadata_last_updated = as.POSIXct(metadata_last_updated, tz = "UTC"),
    harvest_timestamp = as.POSIXct(harvest_timestamp, tz = "UTC"),
    variables = list(variables),
    lineage = list(lineage),
    quality_score = as.numeric(quality_score)
  )
}

#' Validate canonical metadata structure
#'
#' @param data A data frame in canonical format.
#'
#' @return The input data invisibly if valid.
#' @export
validate_canonical_metadata <- function(data) {
  required <- names(canonical_metadata_template())
  missing <- setdiff(required, names(data))

  if (length(missing)) {
    stop(
      "Missing required canonical columns: ",
      paste(missing, collapse = ", ")
    )
  }

  if (!nrow(data)) {
    return(invisible(data))
  }

  if (any(is.na(data$dataset_id) | data$dataset_id == "")) {
    stop("dataset_id must be present for every record.")
  }

  if (any(is.na(data$dataset_name) | data$dataset_name == "")) {
    stop("dataset_name must be present for every record.")
  }

  if (any(is.na(data$source_catalogue) | data$source_catalogue == "")) {
    stop("source_catalogue must be present for every record.")
  }

  invisible(data)
}

#' @keywords internal
list_column_to_json <- function(x) {
  if (!is.list(x)) {
    return(x)
  }

  vapply(x, jsonify_value, character(1))
}

#' Prepare metadata for storage
#'
#' Convert list columns into JSON strings so the table can be stored in DuckDB
#' and other relational stores.
#'
#' @param data Canonical metadata.
#'
#' @return A flat data frame safe for relational storage.
#' @export
prepare_metadata_for_storage <- function(data) {
  data <- tibble::as_tibble(data)

  for (column in c(
    "keywords",
    "themes",
    "geographic_coverage",
    "temporal_coverage",
    "variables",
    "lineage"
  )) {
    if (column %in% names(data)) {
      data[[column]] <- list_column_to_json(data[[column]])
    }
  }

  data$metadata_last_updated <- as.character(data$metadata_last_updated)
  data$harvest_timestamp <- as.character(data$harvest_timestamp)
  data
}

#' Restore metadata from storage
#'
#' @param data A data frame created by the storage preparation helper.
#'
#' @return Canonical metadata with list columns restored.
#' @export
restore_metadata_from_storage <- function(data) {
  data <- tibble::as_tibble(data)

  decode_json_col <- function(x) {
    if (!length(x)) {
      return(list())
    }

    purrr::map(x, function(value) {
      if (is.na(value) || value == "") {
        return(character())
      }
      jsonlite::fromJSON(value, simplifyVector = TRUE)
    })
  }

  for (column in c(
    "keywords",
    "themes",
    "geographic_coverage",
    "temporal_coverage",
    "variables",
    "lineage"
  )) {
    if (column %in% names(data)) {
      data[[column]] <- decode_json_col(data[[column]])
    }
  }

  if ("metadata_last_updated" %in% names(data)) {
    data$metadata_last_updated <- as.POSIXct(
      data$metadata_last_updated,
      tz = "UTC"
    )
  }

  if ("harvest_timestamp" %in% names(data)) {
    data$harvest_timestamp <- as.POSIXct(data$harvest_timestamp, tz = "UTC")
  }

  data
}

#' Normalise raw records into canonical metadata rows
#'
#' @keywords internal
normalise_raw_records <- function(records, mapper) {
  if (!length(records)) {
    return(canonical_metadata_template())
  }

  purrr::map_dfr(records, mapper) |>
    validate_canonical_metadata()
}
