#' Score metadata quality
#'
#' Apply a transparent 0--100 metadata quality score based on completeness of
#' description, keyword richness, owner attribution, licence, update metadata,
#' and geographic metadata.
#'
#' @param data Canonical metadata.
#'
#' @return Canonical metadata with a `quality_score` column.
#' @export
score_metadata_quality <- function(data) {
  data <- tibble::as_tibble(data)

  score_description <- function(x) {
    n <- nchar(trimws(as.character(x)))
    dplyr::case_when(
      is.na(n) ~ 0,
      n >= 250 ~ 30,
      n >= 120 ~ 24,
      n >= 50 ~ 18,
      n >= 20 ~ 8,
      TRUE ~ 0
    )
  }

  score_keywords <- function(x) {
    n <- vapply(
      x,
      function(value) length(to_character_vector(value)),
      integer(1)
    )
    dplyr::case_when(
      n >= 6 ~ 15,
      n >= 3 ~ 12,
      n >= 1 ~ 6,
      TRUE ~ 0
    )
  }

  score_presence <- function(x, points) {
    ifelse(is.na(x) | trimws(as.character(x)) == "", 0, points)
  }

  score_owner <- function(organisation, publisher) {
    has_owner <- !is.na(organisation) & trimws(as.character(organisation)) != ""
    has_publisher <- !is.na(publisher) & trimws(as.character(publisher)) != ""
    ifelse(has_owner | has_publisher, 15, 0)
  }

  score_geo <- function(x) {
    n <- vapply(
      x,
      function(value) length(to_character_vector(value)),
      integer(1)
    )
    ifelse(n > 0, 10, 0)
  }

  score_update <- function(meta_date, freq) {
    has_date <- !is.na(meta_date)
    has_freq <- !is.na(freq) & trimws(as.character(freq)) != ""
    dplyr::case_when(
      has_date & has_freq ~ 15,
      has_date | has_freq ~ 8,
      TRUE ~ 0
    )
  }

  data$quality_score <- score_description(data$description) +
    score_keywords(data$keywords) +
    score_owner(data$organisation, data$publisher) +
    score_presence(data$licence, 15) +
    score_update(data$metadata_last_updated, data$update_frequency) +
    score_geo(data$geographic_coverage)

  data$quality_score <- pmin(pmax(data$quality_score, 0), 100)
  data
}

#' Produce a metadata quality report
#'
#' @param data Canonical metadata.
#'
#' @return A list containing summary and record-level quality information.
#' @export
quality_report <- function(data) {
  data <- score_metadata_quality(data)

  summary <- tibble::tibble(
    record_count = nrow(data),
    mean_quality_score = if (nrow(data)) {
      mean(data$quality_score, na.rm = TRUE)
    } else {
      NA_real_
    },
    median_quality_score = if (nrow(data)) {
      stats::median(data$quality_score, na.rm = TRUE)
    } else {
      NA_real_
    },
    low_quality_records = sum(data$quality_score < 50, na.rm = TRUE)
  )

  breakdown <- tibble::tibble(
    rule = c(
      "description",
      "keywords",
      "owner",
      "licence",
      "update",
      "geography"
    ),
    weight = c(30, 15, 15, 15, 15, 10)
  )

  list(
    summary = summary,
    breakdown = breakdown,
    records = data
  )
}

#' Write a quality report to disk
#'
#' @param report Output from the quality reporting helper.
#' @param path Output directory.
#'
#' @return Invisibly returns the output directory.
#' @export
write_quality_report <- function(report, path) {
  ensure_directory(path)

  utils::write.csv(
    report$summary,
    file.path(path, "quality_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    report$breakdown,
    file.path(path, "quality_breakdown.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    report$records |>
      dplyr::select(
        record_type,
        dataset_id,
        dataset_name,
        source_catalogue,
        quality_score
      ),
    file.path(path, "quality_records.csv"),
    row.names = FALSE
  )

  md <- c(
    "# Metadata quality report",
    "",
    sprintf("- Records: %s", report$summary$record_count[[1]]),
    sprintf("- Mean score: %.1f", report$summary$mean_quality_score[[1]]),
    sprintf("- Median score: %.1f", report$summary$median_quality_score[[1]]),
    sprintf(
      "- Records below threshold: %s",
      report$summary$low_quality_records[[1]]
    ),
    "",
    "## Score weights",
    "",
    paste0("- ", report$breakdown$rule, ": ", report$breakdown$weight)
  )
  writeLines(md, file.path(path, "quality_report.md"))
  invisible(path)
}
