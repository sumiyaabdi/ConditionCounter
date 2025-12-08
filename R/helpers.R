getConceptTable <- function(connection) {
  conceptDf <- DatabaseConnector::querySql(connection, "SELECT * FROM concept")
  return(conceptDf)
}
getConditionText <- function(conditionId, conceptTable) {
  stopifnot(all(c("concept_id", "concept_name") %in% names(conceptTable)))
  stopifnot(conditionId %in% conceptTable$concept_id)

  map_vec <- stats::setNames(conceptTable$concept_name, conceptTable$concept_id)

  return(unname(map_vec[as.character(conditionId)]))
}
