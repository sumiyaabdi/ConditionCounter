plotTrend <- function(extractedCounts, byMonth = FALSE, condition_concept_id = NULL) {
  stopifnot(
    all(c("condition_concept_id", "year", "month", "n_patients") %in% names(extractedCounts))
    )

  if (!is.null(condition_concept_id)) {
    extractedCounts <- extractedCounts[extractedCounts['condition_concept_id'] == condition_concept_id, ]
  }

  if (byMonth) {
    extractedCounts$year_month <- paste(extractedCounts$year, extractedCounts$month, sep = "_")
    xlabel <- "year_month"
  }
  else {
    extractedCounts <- extractedCounts |>
      dplyr::group_by(condition_concept_id, year) |>
      dplyr::summarise(n_patients = sum(n_patients), .groups = "drop")

    xlabel <- "year"
  }

  ggplot(data = extractedCounts, aes(x = year, y = n_patients, color = condition_concept_id, group = condition_concept_id)) +
    geom_line() +
    geom_point() +
}
