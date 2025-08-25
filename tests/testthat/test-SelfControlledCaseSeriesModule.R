library(testthat)
library(Strategus)

test_that("SelfControlledCaseSeriesModule - create module settings compatible for SCCS v6", {
  sccsModule <- SelfControlledCaseSeriesModule$new()
  expect_error(
    sccsModule$createModuleSpecifications(
      sccsAnalysisList = list()
    )
  )

  expect_error(
    sccsModule$createModuleSpecifications(
      exposuresOutcomeList = list()
    )
  )

  expect_error(
    sccsModule$createModuleSpecifications(
      combineDataFetchAcrossOutcomes = FALSE
    )
  )

  expect_error(
    sccsModule$createModuleSpecifications(
      sccsDiagnosticThresholds = list()
    )
  )
})
