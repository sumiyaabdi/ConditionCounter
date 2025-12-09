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
#' @param conditionName Optional list of condition names
#'
#' @return A data.frame containing `x`, `n_patients`,
#'   and `condition_concept_id`.
#'
#' @keywords internal
prepareTrends <- function(occurrences,
                          byMonth = FALSE,
                          conditionConceptId = NULL,
                          conditionName = NULL) {

  stopifnot(all(c("condition_concept_id", "year", "n_patients") %in% names(occurrences)))
  stopifnot (!(!is.null(conditionConceptId) && !is.null(conditionName))) # can't filter with both name & id

  # filter by condition_concept_id
  if (!is.null(conditionConceptId)) {
    occurrences <- occurrences |>
      dplyr::filter(condition_concept_id %in% conditionConceptId) |>
      dplyr::rename(grouping = condition_concept_id)
  }

  # filter by condition name
  if (!is.null(conditionName) && "concept_name" %in% names(occurrences)) {
    occurrences <- occurrences |>
      dplyr::filter(concept_name %in% conditionName) |>
      dplyr::rename(grouping = concept_name)
  }

  # rename column if not being filtered
  if (!"grouping" %in% names(occurrences)) {
    occurrences <- occurrences |>
      dplyr::rename(grouping = condition_concept_id)
  }

  # aggregate over selected time
  if (byMonth) {
    stopifnot("month" %in% names(occurrences))
    trends <- occurrences |>
      dplyr::mutate(x = as.Date(sprintf("%04d-%02d-01", year, month))) |>
      dplyr::select(x, n_patients, grouping)
  }
  else {
    trends <- occurrences |>
      dplyr::group_by(grouping, year) |>
      dplyr::summarise(n_patients = sum(n_patients), .groups = "drop") |>
      dplyr::mutate(x = year) |>
      dplyr::select(x, n_patients, grouping)
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
                 color = as.factor(.data$grouping),
                 group = .data$grouping)
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Time",
      y = "Number of Patients",
      color = "Condition"
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
#' @param conditionName Optional. One or more `concept_names` from the OMOP CDM `concept` table
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
                      conditionConceptId = NULL,
                      conditionName = NULL) {

  trends <- prepareTrends(
    occurrences = occurrences,
    byMonth = byMonth,
    conditionConceptId = conditionConceptId,
    conditionName = conditionName
  )

  plotPreparedTrends(trends)
}
