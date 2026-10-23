test_that("registry parsing and connector creation work", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(
    c(
      "catalogues:",
      "  - id: example_ckan",
      "    type: ckan",
      "    name: Example CKAN",
      "    base_url: https://example.org",
      "    api_base_url: https://api.example.org",
      "    enabled: true"
    ),
    tmp
  )

  registry <- catalogue_registry(tmp)
  connectors <- create_connectors(registry)

  expect_equal(nrow(registry), 1L)
  expect_s3_class(connectors[[1]], "ckan_connector")
  expect_equal(connectors[[1]]$config$api_base_url, "https://api.example.org")
})

test_that("ckan API URLs can differ from website URLs", {
  conn <- ckan_connector(
    base_url = "https://data.gov.scot",
    api_base_url = "https://api.data.gov.scot"
  )

  expect_equal(
    harvest:::connector_api_url(conn, "/api/3/action/package_list"),
    "https://api.data.gov.scot/api/3/action/package_list"
  )
})
