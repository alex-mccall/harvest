library(harvest)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("reports", showWarnings = FALSE, recursive = TRUE)

registry <- catalogue_registry()
connectors <- create_connectors(registry)

result <- harvest_catalogues(
  connectors,
  output_duckdb = "data/metadata.duckdb",
  output_parquet = "data/metadata.parquet",
  quality_report_dir = "reports/quality",
  n_workers = 1
)

write.csv(
  result$report$summary,
  "reports/quality_summary.csv",
  row.names = FALSE
)
