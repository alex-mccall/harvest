library(harvest)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("reports/quality", showWarnings = FALSE, recursive = TRUE)

registry <- harvest::catalogue_registry()
connectors <- harvest::create_connectors(registry)

result <- harvest::harvest_catalogues(
  connectors,
  output_duckdb = "data/metadata.duckdb",
  output_parquet = "data/metadata.parquet",
  quality_report_dir = "reports/quality"
)
