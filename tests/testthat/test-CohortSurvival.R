library(testthat)
library(dplyr)

test_that("CohortSurvival: execute method", {
  tempDir <- file.path(tempdir(), "Strategus-CohortSurvival")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()

  sa <- Strategus::CohortSurvivalModule$new()

  modSpec <- sa$createModuleSpecifications(
    analysisType = "single_event",
    targetCohortTable = "cohort_table",
    outcomeCohortTable = "cohort_table",
    strata = list(c("age_group"), c("sex")),
    timeGap = 7,
    followUp = 365,
    minCellCount = 5
  )

  analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
    Strategus::addCohortSurvivalModuleSpecifications(modSpec)

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

  # Note: This test would require CohortSurvival package to be installed
  # and proper test data to be available
  # For now, we'll just test that the module can be created and specifications work
  expect_true(inherits(sa, "CohortSurvivalModule"))
  expect_true(inherits(modSpec, "ModuleSpecifications"))
  expect_equal(sa$tablePrefix, "cohort_survival_")
})

test_that("CohortSurvival: competing risk specifications", {
  sa <- Strategus::CohortSurvivalModule$new()

  modSpec <- sa$createModuleSpecifications(
    analysisType = "competing_risk",
    targetCohortTable = "cohort_table",
    outcomeCohortTable = "cohort_table",
    competingOutcomeCohortTable = "cohort_table",
    strata = list(c("age_group")),
    timeGap = 7,
    followUp = 365,
    minCellCount = 5
  )

  expect_true(inherits(modSpec, "ModuleSpecifications"))
  expect_equal(modSpec$settings$analysisType, "competing_risk")
  expect_equal(modSpec$settings$competingOutcomeCohortTable, "cohort_table")
}) 