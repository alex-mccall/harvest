# harvest

Production-oriented metadata harvesting and discovery for Scottish public
sector catalogues.

## What it does

- Harvests metadata only, not underlying datasets.
- Normalises heterogeneous catalogue records into a canonical model.
- Scores metadata quality.
- Stores results in DuckDB and Parquet.
- Provides fuzzy search by catalogue, theme, organisation, and relatedness.

## Supported sources

- CKAN: Data.gov.scot, NHS Open Data, Open Data Scotland.
- Find.Data.Gov.Scot: dataset and organisation discovery.
- Research Data Scotland metadata catalogue.
- SpatialData.gov.scot via GeoNetwork/CSW style APIs.
- NatureScot Spatial Hub via ArcGIS REST / Hub APIs.
- Statistics.gov.scot via SPARQL / RDF.

## Quick start

```r
library(harvest)

registry <- catalogue_registry()
connectors <- create_connectors(registry)

result <- harvest_catalogues(
  connectors,
  output_duckdb = "data/metadata.duckdb",
  output_parquet = "data/metadata.parquet",
  quality_report_dir = "reports/quality"
)
```

## Architecture

```mermaid
flowchart LR
  Registry --> Factory --> Connectors --> CanonicalModel --> Quality --> Storage --> Search
```

