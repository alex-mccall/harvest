#' Store metadata in DuckDB
#'
#' @param data Canonical metadata.
#' @param path Path to a DuckDB database file.
#' @param table Table name.
#' @param overwrite Replace any existing table.
#'
#' @return Invisibly returns the database path.
#' @export
store_metadata_duckdb <- function(
  data,
  path,
  table = "metadata",
  overwrite = TRUE
) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("duckdb is required to store metadata in DuckDB.")
  }

  ensure_directory(dirname(path))
  storage <- prepare_metadata_for_storage(data)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, table, storage, overwrite = overwrite)
  invisible(path)
}

#' Read metadata from DuckDB
#'
#' @param path Path to a DuckDB database file.
#' @param table Table name.
#'
#' @return A canonical metadata tibble.
#' @export
read_metadata_duckdb <- function(path, table = "metadata") {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    stop("duckdb is required to read metadata from DuckDB.")
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  data <- DBI::dbReadTable(con, table)
  restore_metadata_from_storage(data)
}

#' Export metadata to Parquet
#'
#' @param data Canonical metadata.
#' @param path Output path to a Parquet file.
#'
#' @return Invisibly returns the output path.
#' @export
export_metadata_parquet <- function(data, path) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("arrow is required to export metadata to Parquet.")
  }

  ensure_directory(dirname(path))
  arrow::write_parquet(data, path)
  invisible(path)
}

#' Deduplicate metadata records
#'
#' @param data Canonical metadata.
#' @param by Columns used to identify duplicates.
#'
#' @return Deduplicated metadata.
#' @export
deduplicate_metadata <- function(
  data,
  by = c("dataset_id", "source_catalogue", "source_url")
) {
  data <- tibble::as_tibble(data)
  if (!nrow(data)) {
    return(data)
  }

  if (!all(by %in% names(data))) {
    stop("Deduplication columns are missing from metadata.")
  }

  data |>
    dplyr::arrange(
      dplyr::desc(quality_score),
      dplyr::desc(metadata_last_updated),
      dplyr::desc(harvest_timestamp)
    ) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(by)), .keep_all = TRUE)
}

#' Search metadata catalogue
#'
#' @param metadata Canonical metadata, a DuckDB path, or a DBI connection.
#' @param query Search text.
#' @param fields Fields used for fuzzy matching.
#' @param max_results Maximum number of records to return.
#' @param fuzzy Whether to use fuzzy matching.
#'
#' @return A ranked tibble of matching datasets.
#' @export
search_catalogue <- function(
  metadata,
  query,
  fields = c(
    "dataset_name",
    "description",
    "keywords",
    "themes",
    "organisation",
    "publisher"
  ),
  max_results = 20,
  fuzzy = TRUE
) {
  data <- resolve_metadata_source(metadata)
  if (!nrow(data)) {
    return(data)
  }

  query <- normalize_text(query)
  if (is_empty_string(query)) {
    return(
      data |>
        dplyr::arrange(dplyr::desc(quality_score)) |>
        utils::head(max_results)
    )
  }

  fields <- intersect(fields, names(data))
  if (!length(fields)) {
    return(data[0, , drop = FALSE])
  }

  search_blob <- build_search_blob(data, fields)
  search_blob[is.na(search_blob)] <- ""
  exact_score <- ifelse(grepl(query, search_blob, fixed = TRUE), 1, 0)
  fuzzy_score <- vapply(
    search_blob,
    function(x) {
      max_len <- max(nchar(query), nchar(x), 1)
      1 - utils::adist(query, x) / max_len
    },
    numeric(1)
  )

  score <- if (fuzzy) pmax(exact_score, fuzzy_score) else exact_score
  out <- data |>
    dplyr::mutate(search_score = score) |>
    dplyr::filter(.data$search_score > 0) |>
    dplyr::arrange(
      dplyr::desc(.data$search_score),
      dplyr::desc(.data$quality_score)
    ) |>
    utils::head(max_results)
  out
}

#' Search metadata by theme
#'
#' @param metadata Canonical metadata or storage source.
#' @param theme Theme text.
#' @param max_results Maximum number of records to return.
#'
#' @return Matching records.
#' @export
search_by_theme <- function(metadata, theme, max_results = 20) {
  data <- resolve_metadata_source(metadata)
  theme <- normalize_text(theme)
  if (is_empty_string(theme)) {
    return(utils::head(data, max_results))
  }

  search_catalogue(
    data,
    theme,
    fields = c("themes", "keywords", "dataset_name", "description"),
    max_results = max_results
  )
}

#' Search metadata by organisation
#'
#' @param metadata Canonical metadata or storage source.
#' @param organisation Organisation text.
#' @param max_results Maximum number of records to return.
#'
#' @return Matching records.
#' @export
search_by_organisation <- function(metadata, organisation, max_results = 20) {
  data <- resolve_metadata_source(metadata)
  organisation <- normalize_text(organisation)
  if (is_empty_string(organisation)) {
    return(utils::head(data, max_results))
  }

  search_catalogue(
    data,
    organisation,
    fields = c("organisation", "publisher", "dataset_name", "description"),
    max_results = max_results
  )
}

#' Find related datasets
#'
#' @param metadata Canonical metadata or storage source.
#' @param dataset_id Dataset identifier to use as the seed record.
#' @param max_results Maximum number of related records to return.
#'
#' @return A ranked tibble of related records.
#' @export
find_related_datasets <- function(metadata, dataset_id, max_results = 10) {
  data <- resolve_metadata_source(metadata)
  if (!nrow(data)) {
    return(data)
  }

  seed <- data |>
    dplyr::filter(.data$dataset_id == dataset_id) |>
    utils::head(1)

  if (!nrow(seed)) {
    stop(sprintf("dataset_id not found: %s", dataset_id))
  }

  seed_tokens <- unique(c(
    tokenise(seed$dataset_name),
    tokenise(seed$description),
    tokenise(seed$organisation),
    tokenise(seed$publisher),
    tokenise(seed$keywords[[1]]),
    tokenise(seed$themes[[1]])
  ))

  scored <- data |>
    dplyr::filter(.data$dataset_id != dataset_id) |>
    dplyr::mutate(
      related_score = purrr::pmap_dbl(
        dplyr::pick(dplyr::everything()),
        function(...) {
          record <- tibble::as_tibble_row(list(...))
          score_related_record(record, seed_tokens)
        }
      )
    ) |>
    dplyr::filter(.data$related_score > 0) |>
    dplyr::arrange(
      dplyr::desc(.data$related_score),
      dplyr::desc(.data$quality_score)
    ) |>
    utils::head(max_results)

  scored
}

#' @keywords internal
resolve_metadata_source <- function(metadata) {
  if (inherits(metadata, "DBIConnection")) {
    if (!requireNamespace("duckdb", quietly = TRUE)) {
      stop("duckdb is required to read metadata from DuckDB.")
    }

    data <- DBI::dbReadTable(metadata, "metadata")
    return(restore_metadata_from_storage(data))
  }

  if (
    is.character(metadata) && length(metadata) == 1 && file.exists(metadata)
  ) {
    return(read_metadata_duckdb(metadata))
  }

  tibble::as_tibble(metadata)
}

#' @keywords internal
build_search_blob <- function(data, fields) {
  if (!length(fields)) {
    return(rep("", nrow(data)))
  }

  purrr::pmap_chr(data[fields], function(...) {
    values <- list(...)
    collapse_text(
      unlist(values, recursive = TRUE, use.names = FALSE),
      sep = " "
    )
  })
}

#' @keywords internal
score_related_record <- function(record, seed_tokens) {
  record_tokens <- unique(c(
    tokenise(record$dataset_name),
    tokenise(record$description),
    tokenise(record$organisation),
    tokenise(record$publisher),
    tokenise(record$keywords[[1]]),
    tokenise(record$themes[[1]])
  ))

  if (!length(record_tokens) || !length(seed_tokens)) {
    return(0)
  }

  overlap <- length(intersect(record_tokens, seed_tokens))
  union <- length(unique(c(record_tokens, seed_tokens)))
  jaccard <- if (union) overlap / union else 0

  record_title <- tokenise(record$dataset_name)
  record_title <- if (length(record_title)) record_title[1] else ""
  seed_title <- if (length(seed_tokens)) seed_tokens[1] else ""
  title_score <- if (
    is_empty_string(seed_title) || is_empty_string(record_title)
  ) {
    0
  } else {
    max(
      0,
      1 - utils::adist(seed_title, record_title) / max(nchar(seed_title), 1)
    )
  }
  organisation_bonus <- ifelse(
    !is.na(record$organisation) &&
      length(seed_tokens) &&
      any(seed_tokens %in% tokenise(record$organisation)),
    0.15,
    0
  )
  min(1, (0.7 * jaccard) + (0.15 * title_score) + organisation_bonus)
}
