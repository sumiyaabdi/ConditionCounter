test_that("getConditionOccurrence has correct columns", {
  skip_on_cran()

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- DatabaseConnector::connect(connectionDetails)

  df <- getConditionOccurrence(connection)

  expect_true(is.data.frame(df))
  expect_true(all(c("condition_concept_id", "condition_start_date") %in% colnames(df)))
  expect_true(nrow(df) > 0)
})
