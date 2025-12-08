#' Prepare occurrence data for plotting
#'
#' Internal helper that cleans and aggregates condition occurrence counts
#' prior to plotting. Most users should instead call [plotTrend()].
#'
#' @param occurrences A data.frame containing the columns:
#'   `condition_concept_id`, `year`, `month`, `n_patients`.
#' @param byMonth Logical. If TRUE, output one row per year-month combination.
#'   If FALSE, rows are aggregated by year.
#' @param conditionConceptId Optional vector of condition concept IDs to filter.
#'
#' @return A data.frame containing `x`, `n_patients`,
#'   and `condition_concept_id`.
#'
#' @keywords internal
prepareTrends <- function(occurrences,
                             byMonth = FALSE,
                             conditionConceptId = NULL) {
  stopifnot(all(c("condition_concept_id", "year", "n_patients") %in% names(occurrences)))

  if (!is.null(conditionConceptId)) {
    occurrences <- occurrences |>
      dplyr::filter(condition_concept_id %in% conditionConceptId)
  }

  if (byMonth) {
    stopifnot("month" %in% names(occurrences))
    trends <- occurrences |>
      dplyr::mutate(x = paste(year, month, sep = "_")) |>
      dplyr::select(x, n_patients, condition_concept_id)
  }
  else {
    trends <- occurrences |>
      dplyr::group_by(condition_concept_id, year) |>
      dplyr::summarise(n_patients = sum(n_patients), .groups = "drop") |>
      dplyr::mutate(x = year) |>
      dplyr::select(x, n_patients, condition_concept_id)
  }

  return(trends)
}

#' Plot prepared trend data
#'
#' Internal helper used by [plotTrend()]. Produces a ggplot time-trend
#' visualization from data prepared by [prepareTrends()].
#'
#' @param trends A data.frame produced by [prepareTrends()].
#'
#' @return A ggplot object.
#'
#' @keywords internal
plotPreparedTrends <- function(trends) {

  ggplot2::ggplot(
    data = trends,
    ggplot2::aes(x = .data$x,
                 y = .data$n_patients,
                 color = as.factor(.data$condition_concept_id),
                 group = .data$condition_concept_id)
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Time",
      y = "Number of Patients",
      color = "Condition Concept ID"
    )
}

#' Plot condition occurrence trends
#'
#' Takes output from the `extractPatients()` function as input and returns a plot with the number of patients per year (default) or per month for each condition in "data".
#' Internally calls `prepareTrends()` and `plotPreparedTrends()`.
#'
#' @param occurrences A data.frame with
#'   `condition_concept_id`, `year`, `month`, `n_patients`.
#' @param byMonth Logical. If TRUE, plot by year_month; otherwise by year.
#' @param conditionConceptId Optional. One or more concept IDs to filter.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   condition_concept_id = c(10, 10, 10, 20, 20, 20),
#'   year  = c(2020, 2020, 2021, 2020, 2021, 2021),
#'   month = c(1, 2, 1, 3, 1, 2),
#'   n_patients = c(5, 8, 3, 10, 4, 7)
#' )
#'
#' # Prepare monthly data
#' plotTrend(df, byMonth = TRUE)
#'
#' # Prepare yearly aggregated data
#' plotTrend(df, byMonth = FALSE)
#'
#' # Filter to a single concept
#' plotTrend(df, conditionConceptId = 10)
plotTrend <- function(occurrences,
                      byMonth = FALSE,
                      conditionConceptId = NULL) {

  trends <- prepareTrends(
    occurrences = occurrences,
    byMonth = byMonth,
    conditionConceptId = conditionConceptId
  )

  plotPreparedTrends(trends)
}
