test_that("getConditionOccurrence has correct columns", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- DatabaseConnector::connect(connectionDetails)

  df <- getConditionOccurrence(connection)

  expect_true(is.data.frame(df))
  expect_true(all(c("condition_concept_id", "condition_start_date") %in% colnames(df)))
  expect_true(nrow(df) > 0)
  DatabaseConnector::disconnect(connection)
})

test_that("extractPatients returns counts", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- DatabaseConnector::connect(connectionDetails)

  df <- extractPatients(connection)

  expect_true(is.data.frame(df))
  expect_true(all(c("condition_concept_id", "year", "month", "n_patients") %in% colnames(df)))
  expect_type(df$year, "double")
  expect_type(df$month, "double")
  expect_true(all(df$n_patients > 0))
  expect_true(nrow(df) > 0)

  DatabaseConnector::disconnect(connection)
})
