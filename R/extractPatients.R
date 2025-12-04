#' Get condition occurrence data.frame from CDM connection
#'
#' @param connection A DatabaseConnector connection object
#' @param cdmSchema
#'
#' @returns
#' @export
#'
#' @examples
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
  aggregate(
    x = list(n_patients = df$condition_concept_id),
    by = list(
      condition_concept_id = df$condition_concept_id,
      year = df$year,
      month = df$month
    ),
    FUN = length
  )
}

#' get condition occurrences from databse
#'
#' @param connection DatabaseConnector connection
#'
#' This function takes a database connection as input, and extract counts of all conditions by year and month from the condition occurence table in the OMOP CDM database. The function is able to do this for different sql dialects using the SqlRender R package. It should return a data.frame with columns condition_concept_id, year, month, and n_patients
#'
#' @returns data.frame with columns: condition_concept_id, year, month, and n_patients
#' @export
#'
#' @examples
extractPatients <- function(connection) {
  df <- getConditionOccurrence(connection)
  df <- processDates(df)
  df_counts <- countPatients(df)
  return(df_counts)
}
