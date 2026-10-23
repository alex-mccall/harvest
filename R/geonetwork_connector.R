#' Create a GeoNetwork connector
#'
#' @param base_url Base URL of the GeoNetwork instance.
#' @param name Catalogue name.
#' @param config Optional configuration list.
#'
#' @return A connector object.
#' @export
geonetwork_connector <- function(
  base_url,
  name = "SpatialData.gov.scot",
  config = list()
) {
  new_connector("geonetwork", name = name, base_url = base_url, config = config)
}

#' @keywords internal
geonetwork_search_url <- function(x) {
  connector_source_url(x, "/geonetwork/srv/api/search/records/_search")
}

#' @keywords internal
geonetwork_csw_url <- function(x) {
  connector_source_url(x, "/geonetwork/srv/eng/csw")
}

#' @keywords internal
geonetwork_record_url <- function(x, id) {
  connector_source_url(x, paste0("/geonetwork/srv/api/records/", id))
}

#' @keywords internal
geonetwork_scalar_text <- function(x) {
  values <- to_character_vector(x)
  if (length(values)) {
    values[[1]]
  } else {
    NA_character_
  }
}

#' @keywords internal
geonetwork_q_search_url <- function(x) {
  connector_source_url(x, "/geonetwork/srv/eng/q")
}

#' @keywords internal
geonetwork_extract_search_records <- function(body) {
  if (!is.list(body)) {
    return(list())
  }

  if (length(body$records)) {
    return(body$records)
  }

  if (length(body$features)) {
    return(body$features)
  }

  if (length(body$hits$hits)) {
    return(purrr::map(body$hits$hits, function(hit) {
      hit$`_source` %||% hit
    }))
  }

  if (length(body$metadataRecords)) {
    return(body$metadataRecords)
  }

  if (length(body$metadata)) {
    return(body$metadata)
  }

  list()
}

#' @keywords internal
geonetwork_search_records <- function(x, q = "", from = 1, size = 100) {
  query_text <- if (is_empty_string(q)) "*" else q

  csw <- tryCatch(
    geonetwork_csw_search_records(x, q = query_text, from = from, size = size),
    error = function(e) NULL
  )

  if (!is.null(csw) && length(csw)) {
    return(csw)
  }

  modern <- tryCatch(
    request_json_post(
      geonetwork_search_url(x),
      body = list(
        from = from - 1,
        size = size,
        query = list(query_string = list(query = query_text))
      ),
      headers = list(Accept = "application/json")
    ),
    error = function(e) NULL
  )

  if (!is.null(modern)) {
    records <- geonetwork_extract_search_records(modern)
    if (length(records)) {
      return(records)
    }
  }

  legacy <- tryCatch(
    request_json(
      geonetwork_q_search_url(x),
      query = list(
        any = query_text,
        from = from,
        to = from + size - 1,
          `_content_type` = "json",
        fast = "index"
      ),
      headers = list(Accept = "application/json")
    ),
    error = function(e) NULL
  )

  if (!is.null(legacy)) {
    records <- geonetwork_extract_search_records(legacy)
    if (length(records)) {
      return(records)
    }
  }

  list()
}

#' @keywords internal
geonetwork_csw_search_records <- function(x, q = "*", from = 1, size = 100) {
  query <- list(
    service = "CSW",
    version = "2.0.2",
    request = "GetRecords",
    resultType = "results",
    typeNames = "csw:Record",
    outputSchema = "http://www.opengis.net/cat/csw/2.0.2",
    elementSetName = "summary",
    startPosition = from,
    maxRecords = size
  )

  if (!is_empty_string(q) && q != "*") {
    query$constraintLanguage <- "CQL_TEXT"
    query$constraint <- paste0("AnyText like '%", gsub("'", "''", q, fixed = TRUE), "%'")
  }

  body <- tryCatch(
    request_text(
      geonetwork_csw_url(x),
      query = query,
      headers = list(Accept = "application/xml")
    ),
    error = function(e) NULL
  )

  if (is.null(body) || is_empty_string(body)) {
    return(list())
  }

  doc <- tryCatch(xml2::read_xml(body), error = function(e) NULL)
  if (is.null(doc)) {
    return(list())
  }

  nodes <- xml2::xml_find_all(doc, "//*[local-name()='Record']")
  if (!length(nodes)) {
    nodes <- xml2::xml_find_all(doc, "//*[local-name()='MD_Metadata']")
  }

  if (!length(nodes)) {
    return(list())
  }

  purrr::map(nodes, geonetwork_parse_csw_record)
}

#' @keywords internal
geonetwork_parse_csw_record <- function(node) {
  get_text <- function(xpaths) {
    for (xpath in xpaths) {
      value <- xml2::xml_find_first(node, xpath)
      text <- xml2::xml_text(value, trim = TRUE)
      if (!is.na(text) && nzchar(text)) {
        return(text)
      }
    }
    NA_character_
  }

  keywords <- xml2::xml_find_all(node, "//*[local-name()='subject']") |>
    xml2::xml_text(trim = TRUE)
  keywords <- keywords[nzchar(keywords)]

  list(
    uuid = get_text(c(
      ".//*[local-name()='identifier']",
      ".//*[local-name()='fileIdentifier']"
    )),
    id = get_text(c(
      ".//*[local-name()='identifier']",
      ".//*[local-name()='fileIdentifier']"
    )),
    title = get_text(c(
      ".//*[local-name()='title']",
      ".//*[local-name()='name']"
    )),
    displayTitle = get_text(c(
      ".//*[local-name()='title']",
      ".//*[local-name()='name']"
    )),
    description = get_text(c(
      ".//*[local-name()='abstract']",
      ".//*[local-name()='description']"
    )),
    abstract = get_text(c(
      ".//*[local-name()='abstract']",
      ".//*[local-name()='description']"
    )),
    keywords = keywords,
    keyword = keywords,
    publisher = get_text(c(
      ".//*[local-name()='publisher']",
      ".//*[local-name()='organisationName']"
    )),
    modified = get_text(c(
      ".//*[local-name()='modified']",
      ".//*[local-name()='dateStamp']",
      ".//*[local-name()='date']"
    )),
    theme = keywords,
    spatial = get_text(c(
      ".//*[local-name()='spatial']",
      ".//*[local-name()='geographicElement']"
    )),
    temporal = get_text(c(
      ".//*[local-name()='temporal']",
      ".//*[local-name()='temporalElement']"
    )),
    license = get_text(c(
      ".//*[local-name()='license']",
      ".//*[local-name()='useLimitation']"
    ))
  )
}

#' @export
discover.geonetwork_connector <- function(x, q = "", size = 100, ...) {
  records <- tryCatch(
    geonetwork_search_records(x, q = q, size = size),
    error = function(e) {
      log_warn("%s search failed: %s", connector_label(x), conditionMessage(e))
      list()
    }
  )

  if (!length(records)) {
    return(tibble::tibble(
      record_type = character(),
      dataset_id = character(),
      dataset_name = character(),
      source_catalogue = character(),
      source_url = character()
    ))
  }

  purrr::map_dfr(records, function(record) {
    id <- geonetwork_scalar_text(
      record$uuid %||%
        record$id %||%
        record$identifier %||%
        record$metadataIdentifier
    )
    title <- geonetwork_scalar_text(
      record$title %||%
        record$displayTitle %||%
        record$resourceTitle %||%
        record$metadata$identificationInfo$title %||%
        record$resourceTitleObject$default
    )

    if (is_empty_string(id) && is_empty_string(title)) {
      return(canonical_metadata_template())
    }

    if (is_empty_string(id)) {
      id <- title
    }

    if (is_empty_string(title)) {
      title <- id
    }
    tibble::tibble(
      record_type = "dataset",
      dataset_id = id,
      dataset_name = title,
      source_catalogue = x$name,
      source_url = if (!is.na(id)) {
        geonetwork_record_url(x, id)
      } else {
        NA_character_
      }
    )
  }) |>
    dplyr::filter(!is.na(.data$dataset_id))
}

#' @keywords internal
geonetwork_describe_record <- function(x, id) {
  csw_body <- tryCatch(
    request_text(
      geonetwork_csw_url(x),
      query = list(
        service = "CSW",
        version = "2.0.2",
        request = "GetRecordById",
        id = id,
        outputSchema = "http://www.opengis.net/cat/csw/2.0.2"
      ),
      headers = list(Accept = "application/xml")
    ),
    error = function(e) NULL
  )

  if (!is.null(csw_body) && !is_empty_string(csw_body)) {
    doc <- tryCatch(xml2::read_xml(csw_body), error = function(e) NULL)
    if (!is.null(doc)) {
      node <- xml2::xml_find_first(doc, "//*[local-name()='Record']")
      if (!inherits(node, "xml_missing")) {
        return(geonetwork_parse_csw_record(node))
      }

      node <- xml2::xml_find_first(doc, "//*[local-name()='MD_Metadata']")
      if (!inherits(node, "xml_missing")) {
        return(geonetwork_parse_csw_record(node))
      }
    }
  }

  modern <- tryCatch(
    request_json(geonetwork_record_url(x, id)),
    error = function(e) NULL
  )

  if (!is.null(modern)) {
    return(modern)
  }

  legacy <- tryCatch(
    request_json(
      connector_source_url(x, paste0("/geonetwork/srv/eng/xml.metadata.get?uuid=", id)),
      headers = list(Accept = "application/xml")
    ),
    error = function(e) NULL
  )

  if (!is.null(legacy)) {
    return(legacy)
  }

  list(uuid = id)
}

#' @keywords internal
geonetwork_extract_keywords <- function(record) {
  kw <- record$keywords %||%
    record$keyword %||%
    record$keywords$keyword %||%
    list()
  if (!length(kw)) {
    return(character())
  }

  if (is.list(kw)) {
    return(purrr::map_chr(kw, function(item) {
      item$value %||% item$label %||% item$text %||% NA_character_
    }))
  }

  as.character(kw)
}

#' @keywords internal
geonetwork_extract_text <- function(record, paths) {
  for (path in paths) {
    keys <- strsplit(path, "\\.")[[1]]
    value <- record
    for (key in keys) {
      if (is.null(value)) {
        break
      }
      value <- value[[key]]
    }
    if (!is.null(value) && !is_empty_string(value)) {
      return(value)
    }
  }
  NA_character_
}

#' @export
get_metadata.geonetwork_connector <- function(x, data, ...) {
  ids <- if (is.data.frame(data)) data$dataset_id else as.character(data)
  ids <- unique(ids)

  purrr::map(ids, function(id) {
    tryCatch(
      geonetwork_describe_record(x, id),
      error = function(e) {
        log_warn(
          "%s record fetch failed for %s: %s",
          connector_label(x),
          id,
          conditionMessage(e)
        )
        list(uuid = id)
      }
    )
  })
}

#' @export
normalise.geonetwork_connector <- function(x, data, ...) {
  if (!length(data)) {
    return(canonical_metadata_template())
  }

  purrr::map_dfr(data, function(record) {
    id <- geonetwork_scalar_text(
      record$uuid %||%
        record$id %||%
        record$identifier %||%
        record$metadataIdentifier
    )
    title <- geonetwork_scalar_text(
      record$title %||%
        record$displayTitle %||%
        geonetwork_extract_text(
        record,
        c(
          "metadata.identificationInfo.citation.title",
          "metadata.identificationInfo.title",
          "identificationInfo.citation.title"
        )
      )
    )

    description <- geonetwork_scalar_text(
      record$abstract %||%
        record$description %||%
        geonetwork_extract_text(
        record,
        c(
          "metadata.identificationInfo.abstract",
          "metadata.identificationInfo.abstract.default",
          "identificationInfo.abstract"
        )
      )
    )

    publisher <- geonetwork_scalar_text(
      record$organisationName %||%
        record$owner %||%
        geonetwork_extract_text(
        record,
        c(
          "metadata.contact.organisationName",
          "metadata.identificationInfo.pointOfContact.organisationName"
        )
      )
    )

    licence <- geonetwork_scalar_text(
      record$license %||%
        record$useConstraints %||%
        geonetwork_extract_text(
        record,
        c(
          "metadata.identificationInfo.resourceConstraints.useLimitation",
          "metadata.identificationInfo.resourceConstraints.licenseLinkage"
        )
      )
    )

    geography <- c(
      record$spatialRepresentationType %||% character(),
      geonetwork_extract_text(
        record,
        c(
          "metadata.identificationInfo.extent.description",
          "metadata.identificationInfo.extent.geographicElement.geographicDescription"
        )
      )
    )

    variables <- record$attributeDescription %||% record$attributes %||% list()

    if (is_empty_string(id) && is_empty_string(title)) {
      return(canonical_metadata_template())
    }

    if (is_empty_string(id)) {
      id <- title
    }

    if (is_empty_string(title)) {
      title <- id
    }

    canonical_metadata_row(
      record_type = "dataset",
      dataset_id = id,
      dataset_name = title %||% id,
      description = description,
      publisher = publisher,
      source_catalogue = x$name,
      source_url = geonetwork_record_url(x, id),
      organisation = publisher,
      keywords = geonetwork_extract_keywords(record),
      themes = c(
        record$topicCategory %||% character(),
        record$theme %||% character()
      ),
      geographic_coverage = geography,
      temporal_coverage = c(
        geonetwork_extract_text(
          record,
          c(
            "metadata.identificationInfo.extent.temporalElement.extent.beginPosition",
            "metadata.identificationInfo.extent.temporalElement.extent.endPosition"
          )
        )
      ),
      access_type = record$access %||%
        record$distributionInfo %||%
        NA_character_,
      access_constraints = record$accessConstraints %||%
        record$otherConstraints %||%
        NA_character_,
      licence = licence,
      update_frequency = record$maintenanceAndUpdateFrequency %||%
        geonetwork_extract_text(
          record,
          c(
            "metadata.identificationInfo.status",
            "metadata.identificationInfo.resourceMaintenance.maintenanceAndUpdateFrequency"
          )
        ),
      metadata_last_updated = record$dateStamp %||%
        record$metadata$dateStamp %||%
        NA_character_,
      harvest_timestamp = Sys.time(),
      variables = list(variables),
      lineage = list(list(
        raw_record = record,
        source_api = geonetwork_search_url(x)
      ))
    )
  }) |>
    validate_canonical_metadata()
}

#' @export
validate.geonetwork_connector <- function(x, data, ...) {
  validate_canonical_metadata(data)
}
