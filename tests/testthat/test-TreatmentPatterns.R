library(testthat)
library(dplyr)

test_that("TreatmentPatterns: execute method", {
  tempDir <- file.path(tempdir(), "Strategus-TP")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()

  tp <- TreatmentPatternsModule$new()

  modSpec <- tp$createModuleSpecifications(
    cohorts = testSettings$cohorts,
    cohortTableName = testSettings$cohortTableName,
    connectionDetails = testSettings$connectionDetails,
    resultSchema = "main",
    tempEmulationSchema = NULL,
    cdmSchema = "main",
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
    cohortTableNames = list("cohort_table"),
    tempEmulationSchema = NULL,
    workFolder = tempDir,
    resultsFolder = tempDir,
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

  expect_true(file.exists(file.path(executionSettings$resultsFolder, "attrition.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "countsAge.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "countsSex.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "countsYear.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "metadata.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "summaryEventDuration.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "treatmentPathways.csv")))
})

test_that("TreatmentPatterns: execute function", {
  tempDir <- file.path(tempdir(), "Strategus-TP")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()

  tp <- TreatmentPatternsModule$new()

  modSpec <- tp$createModuleSpecifications(
    cohorts = testSettings$cohorts,
    cohortTableName = testSettings$cohortTableName,
    connectionDetails = testSettings$connectionDetails,
    resultSchema = "main",
    tempEmulationSchema = NULL,
    cdmSchema = "main",
    includeTreatments = "startDate",
    indexDateOffset = 0,
    minEraDuration = 30,
    splitEventCohorts = NULL,
    splitTime = NULL,
    eraCollapseSize = 14,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5
  )

  analysisSpec <- Strategus::createEmptyAnalysisSpecificiations()

  executionSettings <- Strategus::createCdmExecutionSettings(
    workDatabaseSchema = testSettings$resultSchema,
    cdmDatabaseSchema = testSettings$cdmSchema,
    cohortTableNames = list("cohort_table"),
    tempEmulationSchema = NULL,
    workFolder = tempDir,
    resultsFolder = tempDir,
    logFileName = "log.txt",
    minCellCount = 5,
    incremental = FALSE,
    maxCores = 1
  )

  Strategus::execute(
    connectionDetails = testSettings$connectionDetails,
    analysisSpecifications = addTreatmentPatternsModuleSpecifications(
      analysisSpecifications = analysisSpec,
      moduleSpecifications = modSpec
    ),
    executionSettings = executionSettings
  )

  expect_true(file.exists(file.path(executionSettings$resultsFolder, "attrition.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "countsAge.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "countsSex.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "countsYear.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "metadata.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "summaryEventDuration.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, "treatmentPathways.csv")))
})
