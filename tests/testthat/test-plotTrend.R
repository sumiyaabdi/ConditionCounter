test_that("prepareTrendData errors if required columns missing", {
  df <- data.frame(a = 1, b = 2)

  expect_error(prepareTrendData(df))
})

test_that("prepareTrendData produces monthly x column when byMonth = TRUE", {
  df <- data.frame(
    condition_concept_id = c(10, 10),
    year  = c(2020, 2020),
    month = c(1, 2),
    n_patients = c(5, 7)
  )

  plotDf <- prepareTrendData(df, byMonth = TRUE)

  expect_true(is.data.frame(plotDf))
  expect_equal(plotDf$x, c("2020_1", "2020_2"))
  expect_equal(plotDf$n_patients, c(5, 7))
  expect_equal(plotDf$condition_concept_id, c(10, 10))
})

test_that("prepareTrendData aggregates correctly by year", {
  df <- data.frame(
    condition_concept_id = c(10, 10, 10),
    year  = c(2020, 2020, 2021),
    month = c(1, 2, 1),
    n_patients = c(5, 7, 3)
  )

  out <- prepareTrendData(df, byMonth = FALSE)

  expect_equal(out$x, c(2020, 2021))
  expect_equal(out$n_patients, c(12, 3))
})

test_that("prepareTrendData filters by conditionConceptId", {
  df <- data.frame(
    condition_concept_id = c(10, 20, 10, 20),
    year  = c(2020, 2020, 2021, 2021),
    month = c(1, 1, 1, 1),
    n_patients = c(5, 10, 3, 4)
  )

  out <- prepareTrendData(df, conditionConceptId = 10)
  expect_all_true(out$condition_concept_id == 10)
})

test_that("plotPreparedData returns a ggplot object", {
  df <- data.frame(
    x = c("2020_1", "2020_2"),
    n_patients = c(5, 7),
    condition_concept_id = c(10, 10)
  )

  plt <- plotPreparedData(df)
  expect_true(ggplot2::is_ggplot(plt))
})

test_that("plotTrend produces a ggplot using prepareTrendData", {
  df <- data.frame(
    condition_concept_id = c(10, 10),
    year  = c(2020, 2020),
    month = c(1, 2),
    n_patients = c(5, 7)
  )

  plt <- plotTrend(df, byMonth = TRUE)
  expect_true(ggplot2::is_ggplot(plt))
})
