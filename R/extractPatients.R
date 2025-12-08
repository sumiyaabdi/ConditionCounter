getConditionOccurrence <- function(connection, cdmSchema = NULL) {
  if (is.null(cdmSchema)) {
    sql <- "
      SELECT
        condition_concept_id,
        condition_start_date
      FROM condition_occurrence;
    "
    rendered <- sql
  } else {
    sql <- "
      SELECT
        condition_concept_id,
        condition_start_date
      FROM @cdmSchema.condition_occurrence;
    "
    rendered <- SqlRender::render(sql, cdmSchema = cdmSchema)
  }
  translated <- SqlRender::translate(
    sql = rendered,
    targetDialect = connection@dbms
  )

  DatabaseConnector::querySql(connection, translated)
}

processDates <- function(occurrences) {

  stopifnot("condition_start_date" %in% names(occurrences))

  occurrences$condition_start_date <- as.Date(occurrences$condition_start_date)
  occurrences$year <- as.numeric(format(occurrences$condition_start_date, "%Y"))
  occurrences$month <- as.numeric(format(occurrences$condition_start_date, "%m"))

  return(occurrences)
}

countOccurrences <- function(df) {
  stats::aggregate(
    x = list(n_patients = df$condition_concept_id),
    by = list(
      condition_concept_id = df$condition_concept_id,
      year = df$year,
      month = df$month
    ),
    FUN = length
  )
}

addConditionNames <- function(occurrences, connection) {

  conceptDefinitions <- getConceptTable(connection)

  stopifnot(all(c("concept_id", "concept_name") %in% names(conceptDefinitions)))

  occurrences |>
    dplyr::left_join(conceptDefinitions |>
    dplyr::select(concept_id, concept_name), by = c("condition_concept_id" = "concept_id"))
}

#' Get condition_occurrence table from OMOP CDM connection
#'
#' #' Retrieves the `condition_occurrence` table from a connected OMOP CDM database.
#' SQL is translated using `SqlRender`, allowing support for multiple SQL dialects.
#'
#' @param connection A DatabaseConnector connection object
#' @param cdmSchema Optional. A schema name.
#' @param addNames Optional. Bool whether to add `concept_name` column
#'
#' @returns A data.frame with columns `condition_concept_id`, `year`, `month`, `n_patients` and optionally `concept_name`.
#' @export
#'
#' @examples
#' \dontrun{
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' connection <- DatabaseConnector::connect(connectionDetails)
#'
#' df <- getConditionOccurrence(connection)
#' }
extractPatients <- function(connection,
                            cdmSchema = NULL,
                            addNames = FALSE) {

  occurrences <- connection |>
    getConditionOccurrence(cdmSchema = cdmSchema) |>
    processDates() |>
    countOccurrences()

  if (addNames) {
    occurrences <- addConditionNames(occurrences, connection)
  }

  return(occurrences)
}
