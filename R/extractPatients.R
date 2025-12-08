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

processDates <- function(df) {
  df$condition_start_date <- as.Date(df$condition_start_date)
  df$year <- as.numeric(format(df$condition_start_date, "%Y"))
  df$month <- as.numeric(format(df$condition_start_date, "%m"))
  return(df)
}

countPatients <- function(df) {
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

#' Get condition_occurrence table from OMOP CDM connection
#'
#' #' Retrieves the `condition_occurrence` table (or selected columns)
#' from a connected OMOP CDM database.
#' SQL is translated using `SqlRender`, allowing support for multiple SQL dialects.
#'
#' @param connection A DatabaseConnector connection object
#' @param cdmSchema Optional. A schema name.
#'
#' @returns A data.frame with columns `condition_concept_id`, `year`, `month`, `n_patients`
#' @export
#'
#' @examples
#' \dontrun{
#' connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#' connection <- DatabaseConnector::connect(connectionDetails)
#'
#' df <- getConditionOccurrence(connection)
#' }
extractPatients <- function(connection, cdmSchema = NULL) {
  df <- getConditionOccurrence(connection = connection, cdmSchema = cdmSchema)
  df <- processDates(df)
  df_counts <- countPatients(df)
  return(df_counts)
}
