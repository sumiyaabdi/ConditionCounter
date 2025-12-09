test_that("prepareTrends errors if required columns missing", {
  df <- data.frame(a = 1, b = 2)

  expect_error(prepareTrends(df))
})

test_that("prepareTrends produces monthly x column when byMonth = TRUE", {
  df <- data.frame(
    condition_concept_id = c(10, 10),
    year  = c(2020, 2020),
    month = c(1, 2),
    n_patients = c(5, 7)
  )

  plotDf <- prepareTrends(df, byMonth = TRUE)

  expect_true(is.data.frame(plotDf))
  expect_equal(plotDf$x, as.Date(c("2020-01-01", "2020-02-01")))
  expect_equal(plotDf$n_patients, c(5, 7))
  expect_equal(plotDf$grouping, c(10, 10))
})

test_that("prepareTrends aggregates correctly by year", {
  df <- data.frame(
    condition_concept_id = c(10, 10, 10),
    year  = c(2020, 2020, 2021),
    month = c(1, 2, 1),
    n_patients = c(5, 7, 3)
  )

  out <- prepareTrends(df, byMonth = FALSE)

  expect_equal(out$x, c(2020, 2021))
  expect_equal(out$n_patients, c(12, 3))
})

test_that("prepareTrends filters by conditionConceptId", {
  df <- data.frame(
    condition_concept_id = c(10, 20, 10, 20),
    year  = c(2020, 2020, 2021, 2021),
    month = c(1, 1, 1, 1),
    n_patients = c(5, 10, 3, 4)
  )

  out <- prepareTrends(df, conditionConceptId = 10)
  expect_all_true(out$grouping == 10)
})

test_that("plotPreparedTrends returns a ggplot object", {
  df <- data.frame(
    x = c("2020_1", "2020_2"),
    n_patients = c(5, 7),
    condition_concept_id = c(10, 10)
  )

  plt <- plotPreparedTrends(df)
  expect_true(ggplot2::is_ggplot(plt))
})

test_that("plotTrend produces a ggplot using prepareTrends", {
  df <- data.frame(
    condition_concept_id = c(10, 10),
    year  = c(2020, 2020),
    month = c(1, 2),
    n_patients = c(5, 7)
  )

  plt <- plotTrend(df, byMonth = TRUE)
  expect_true(ggplot2::is_ggplot(plt))
})
