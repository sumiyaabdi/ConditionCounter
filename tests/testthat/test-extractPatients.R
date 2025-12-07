test_that("Eunomia connection works", {
  skip_if_not_installed("Eunomia")
  skip_if_not_installed("DatabaseConnector")
  skip_if_not_installed("SqlRender")

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  expect_true(any(DatabaseConnector::getTableNames(connection) %in% "condition_occurrence"))
  expect_true(all(dim(DatabaseConnector::querySql(connection, "SELECT * FROM condition_occurrence;")) > 0))
})

test_that("getConditionOccurrence has correct columns", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  df <- getConditionOccurrence(connection)

  expect_true(is.data.frame(df))
  expect_true(all(c("condition_concept_id", "condition_start_date") %in% colnames(df)))
  expect_true(nrow(df) > 0)
})

test_that("extractPatients returns counts", {
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  df <- extractPatients(connection)

  expect_true(is.data.frame(df))
  expect_true(all(c("condition_concept_id", "year", "month", "n_patients") %in% colnames(df)))
  expect_type(df$year, "double")
  expect_type(df$month, "double")
  expect_true(all(df$n_patients > 0))
  expect_true(nrow(df) > 0)

})
