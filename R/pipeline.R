#' Harvest all configured catalogues
#'
#' @param connectors A named list of connector objects.
#' @param output_duckdb Optional path to a DuckDB database.
#' @param output_parquet Optional path to a Parquet file.
#' @param quality_report_dir Optional directory for quality outputs.
#' @param n_workers Number of workers for parallel harvesting.
#'
#' @return A list with metadata and quality report objects.
#' @export
harvest_catalogues <- function(
  connectors,
  output_duckdb = NULL,
  output_parquet = NULL,
  quality_report_dir = NULL,
  n_workers = 1
) {
  if (!length(connectors)) {
    return(list(
      metadata = canonical_metadata_template(),
      report = quality_report(canonical_metadata_template())
    ))
  }

  harvest_one <- function(connector) {
    discovered <- discover(connector)
    if (!nrow(discovered)) {
      return(canonical_metadata_template())
    }

    raw <- get_metadata(connector, discovered)
    if (!length(raw)) {
      return(canonical_metadata_template())
    }

    canonical <- normalise(connector, raw)
    validate(connector, canonical)
  }

  log_info("Harvesting %s connectors", length(connectors))
  if (n_workers > 1) {
    if (
      requireNamespace("future", quietly = TRUE) &&
        requireNamespace("furrr", quietly = TRUE)
    ) {
      future::plan(future::multisession, workers = n_workers)
      harvested <- furrr::future_map_dfr(
        connectors,
        harvest_one,
        .options = furrr::furrr_options(seed = TRUE)
      )
    } else {
      harvested <- purrr::map_dfr(connectors, harvest_one)
    }
  } else {
    harvested <- purrr::map_dfr(connectors, harvest_one)
  }

  deduped <- deduplicate_metadata(harvested)
  scored <- score_metadata_quality(deduped)
  report <- quality_report(scored)

  if (!is.null(output_duckdb)) {
    if (requireNamespace("duckdb", quietly = TRUE)) {
      store_metadata_duckdb(scored, output_duckdb)
    } else {
      log_warn("Skipping DuckDB export because duckdb is not installed.")
    }
  }

  if (!is.null(output_parquet)) {
    if (requireNamespace("arrow", quietly = TRUE)) {
      export_metadata_parquet(scored, output_parquet)
    } else {
      log_warn("Skipping Parquet export because arrow is not installed.")
    }
  }

  if (!is.null(quality_report_dir)) {
    write_quality_report(report, quality_report_dir)
  }

  list(metadata = scored, report = report)
}
