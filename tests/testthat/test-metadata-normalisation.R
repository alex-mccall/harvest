test_that("canonical metadata template has expected columns", {
  template <- canonical_metadata_template()

  expect_s3_class(template, "tbl_df")
  expect_equal(
    names(template),
    c(
      "record_type",
      "dataset_id",
      "dataset_name",
      "description",
      "publisher",
      "source_catalogue",
      "source_url",
      "organisation",
      "keywords",
      "themes",
      "geographic_coverage",
      "temporal_coverage",
      "access_type",
      "access_constraints",
      "licence",
      "update_frequency",
      "metadata_last_updated",
      "harvest_timestamp",
      "variables",
      "lineage",
      "quality_score"
    )
  )
})

test_that("storage round trip preserves canonical structure", {
  record <- harvest:::canonical_metadata_row(
    dataset_id = "abc",
    dataset_name = "Example dataset",
    source_catalogue = "Example catalogue",
    keywords = c("housing", "planning"),
    themes = c("places"),
    organisation = "Example Org",
    licence = "OGL"
  )

  storage <- harvest:::prepare_metadata_for_storage(record)
  restored <- harvest:::restore_metadata_from_storage(storage)

  expect_equal(restored$dataset_id, record$dataset_id)
  expect_equal(restored$keywords[[1]], c("housing", "planning"))
  expect_equal(restored$themes[[1]], "places")
})
