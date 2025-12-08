getConceptTable <- function(connection) {
  conceptDf <- querySql(connection, "SELECT * FROM concept")
  return()
}
getConditionText <- function(conditionId, conceptTable) {
  stopifnot(all(c("concept_id", "concept_name") %in% names(conceptTable)))
  stopifnot(conditionId %in% conceptTable$concept_id)

  conditionText <- conceptTable |>
    dplyr::filter(concept_id %in% conditionId) |>
    dplyr::select(concept_name)

  return(conditionText[[1]])
}
