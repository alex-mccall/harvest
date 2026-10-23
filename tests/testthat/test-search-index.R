test_that("deduplication keeps the highest scoring record", {
  one <- harvest:::canonical_metadata_row(
    dataset_id = "dup",
    dataset_name = "Dataset A",
    source_catalogue = "Catalogue A",
    source_url = "https://example.org/a",
    quality_score = 10
  )
  two <- harvest:::canonical_metadata_row(
    dataset_id = "dup",
    dataset_name = "Dataset A revised",
    source_catalogue = "Catalogue A",
    source_url = "https://example.org/a",
    quality_score = 90
  )

  deduped <- deduplicate_metadata(dplyr::bind_rows(one, two))

  expect_equal(nrow(deduped), 1L)
  expect_equal(deduped$quality_score[[1]], 90)
})

test_that("fuzzy search finds relevant records", {
  data <- dplyr::bind_rows(
    harvest:::canonical_metadata_row(
      dataset_id = "health-1",
      dataset_name = "Health indicators",
      description = "Public health indicators for Scotland.",
      source_catalogue = "Example catalogue",
      keywords = c("health", "indicators"),
      quality_score = 80
    ),
    harvest:::canonical_metadata_row(
      dataset_id = "transport-1",
      dataset_name = "Transport usage",
      description = "Transport usage statistics.",
      source_catalogue = "Example catalogue",
      keywords = c("transport"),
      quality_score = 75
    )
  )

  results <- search_catalogue(data, "healh")

  expect_equal(results$dataset_id[[1]], "health-1")
})

test_that("theme and organisation search works", {
  data <- dplyr::bind_rows(
    harvest:::canonical_metadata_row(
      dataset_id = "one",
      dataset_name = "First",
      source_catalogue = "Example catalogue",
      themes = c("environment"),
      organisation = "NatureScot"
    ),
    harvest:::canonical_metadata_row(
      dataset_id = "two",
      dataset_name = "Second",
      source_catalogue = "Example catalogue",
      themes = c("health"),
      organisation = "NHS"
    )
  )

  expect_equal(search_by_theme(data, "environment")$dataset_id[[1]], "one")
  expect_equal(
    search_by_organisation(data, "NatureScot")$dataset_id[[1]],
    "one"
  )
})
