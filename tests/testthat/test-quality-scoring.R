test_that("quality scoring is bounded and rewards completeness", {
  sparse <- harvest:::canonical_metadata_row(
    dataset_id = "sparse",
    dataset_name = "Sparse dataset",
    source_catalogue = "Example catalogue"
  )

  complete <- harvest:::canonical_metadata_row(
    dataset_id = "complete",
    dataset_name = "Complete dataset",
    description = strrep("Long descriptive metadata. ", 8),
    publisher = "Example Org",
    source_catalogue = "Example catalogue",
    keywords = c("one", "two", "three", "four"),
    themes = c("theme"),
    geographic_coverage = c("Scotland"),
    temporal_coverage = c("2020-2024"),
    access_type = "open",
    access_constraints = "none",
    licence = "OGL",
    update_frequency = "monthly",
    metadata_last_updated = Sys.time()
  )

  scored <- score_metadata_quality(dplyr::bind_rows(sparse, complete))

  expect_true(all(scored$quality_score >= 0 & scored$quality_score <= 100))
  expect_gt(scored$quality_score[[2]], scored$quality_score[[1]])
})

test_that("quality report summarises records", {
  data <- harvest:::canonical_metadata_row(
    dataset_id = "one",
    dataset_name = "One",
    source_catalogue = "Example catalogue"
  )

  report <- quality_report(data)

  expect_s3_class(report$summary, "tbl_df")
  expect_equal(report$summary$record_count[[1]], 1L)
})
