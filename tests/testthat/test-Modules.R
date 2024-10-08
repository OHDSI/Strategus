library(testthat)
library(Strategus)

testResultsDataModelFormat <- function(moduleName) {
  module <- get(moduleName)$new()
  rdms <- module$getResultsDataModelSpecification()
  expect_true(all(CohortGenerator::isSnakeCase(rdms$tableName)))
  expect_true(all(CohortGenerator::isSnakeCase(rdms$columnName)))
}

test_that("CharacterizationModule results data model is in correct format", {
  testResultsDataModelFormat("CharacterizationModule")
})

test_that("CohortDiagnosticsModule results data model is in correct format", {
  testResultsDataModelFormat("CohortDiagnosticsModule")
})

test_that("CohortGeneratorModule results data model is in correct format", {
  testResultsDataModelFormat("CohortGeneratorModule")
})

test_that("CohortIncidenceModule results data model is in correct format", {
  testResultsDataModelFormat("CohortIncidenceModule")
})

test_that("CohortMethodModule results data model is in correct format", {
  testResultsDataModelFormat("CohortMethodModule")
})

test_that("EvidenceSynthesisModule results data model is in correct format", {
  testResultsDataModelFormat("EvidenceSynthesisModule")
})

test_that("PatientLevelPredictionModule results data model is in correct format", {
  testResultsDataModelFormat("PatientLevelPredictionModule")
})

test_that("SelfControlledCaseSeriesModule results data model is in correct format", {
  testResultsDataModelFormat("SelfControlledCaseSeriesModule")
})
