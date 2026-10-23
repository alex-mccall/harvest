test_that("statistics.gov.scot endpoint resolves to sparql.json", {
  conn <- sparql_connector("https://statistics.gov.scot/sparql")

  expect_equal(
    harvest:::sparql_query_endpoint(conn),
    "https://statistics.gov.scot/sparql.json"
  )
})

