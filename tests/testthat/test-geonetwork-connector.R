test_that("GeoNetwork CSW records are parsed into a usable shape", {
  xml <- xml2::read_xml(
    '<?xml version="1.0" encoding="UTF-8"?>
    <csw:GetRecordsResponse xmlns:csw="http://www.opengis.net/cat/csw/2.0.2"
      xmlns:dc="http://purl.org/dc/elements/1.1/"
      xmlns:dct="http://purl.org/dc/terms/">
      <csw:SearchResults>
        <csw:Record>
          <dc:identifier>123e4567-e89b-12d3-a456-426614174000</dc:identifier>
          <dc:title>Example spatial dataset</dc:title>
          <dc:subject>transport</dc:subject>
          <dc:description>Example description</dc:description>
          <dct:modified>2024-10-01</dct:modified>
        </csw:Record>
      </csw:SearchResults>
    </csw:GetRecordsResponse>'
  )

  node <- xml2::xml_find_first(xml, "//*[local-name()='Record']")
  parsed <- harvest:::geonetwork_parse_csw_record(node)

  expect_equal(parsed$uuid, "123e4567-e89b-12d3-a456-426614174000")
  expect_equal(parsed$title, "Example spatial dataset")
  expect_equal(parsed$description, "Example description")
  expect_equal(parsed$keywords, "transport")
})

