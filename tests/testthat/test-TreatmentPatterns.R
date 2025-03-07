library(testthat)
library(dplyr)

test_that("TreatmentPatterns: execute method", {
  tempDir <- file.path(tempdir(), "Strategus-TP")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()

  tp <- TreatmentPatternsModule$new()

  modSpec <- tp$createModuleSpecifications(
    cohorts = testSettings$cohorts,
    includeTreatments = "startDate",
    indexDateOffset = 0,
    minEraDuration = 7,
    splitEventCohorts = NULL,
    splitTime = NULL,
    eraCollapseSize = 14,
    combinationWindow = 7,
    minPostCombinationDuration = 7,
    filterTreatments = "First",
    maxPathLength = 5
  )

  analysisSpec <- Strategus::createEmptyAnalysisSpecificiations()

  executionSettings <- Strategus::createCdmExecutionSettings(
    workDatabaseSchema = testSettings$resultSchema,
    cdmDatabaseSchema = testSettings$cdmSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort_table"),
    tempEmulationSchema = NULL,
    workFolder = file.path(tempDir, "work_folder"),
    resultsFolder = file.path(tempDir, "results_folder"),
    logFileName = "log.txt",
    minCellCount = 5,
    incremental = FALSE,
    maxCores = 1
  )

  tp$execute(
    connectionDetails = testSettings$connectionDetails,
    analysisSpecifications = addTreatmentPatternsModuleSpecifications(
      analysisSpecifications = analysisSpec,
      moduleSpecifications = modSpec
    ),
    executionSettings = executionSettings
  )

  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "attrition.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "counts_age.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "counts_sex.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "counts_year.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "metadata.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "summary_event_duration.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "treatment_pathways.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "resultsDataModelSpecification.csv")))
})
