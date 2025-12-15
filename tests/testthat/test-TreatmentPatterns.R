library(testthat)
library(dplyr)

test_that("TreatmentPatterns: execute method", {
  skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))

  tempDir <- file.path(tempdir(), "Strategus-TP")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()

  tp <- Strategus::TreatmentPatternsModule$new()

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

  analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
    Strategus::addTreatmentPatternsModuleSpecifications(modSpec)

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

  Strategus::execute(
    connectionDetails = testSettings$connectionDetails,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )

  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "attrition.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_age.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_sex.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_year.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "metadata.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "summary_event_duration.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "treatment_pathways.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "resultsDataModelSpecification.csv")))
})


test_that("Bad Cohort Names", {
  skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))

  tempDir <- file.path(tempdir(), "Strategus-TP")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()
  tp <- Strategus::TreatmentPatternsModule$new


  expect_error(
    modSpecs <- list(
      tp$createAnalysisSpecification(
        targetCohorts = list(
          list(8, "Viral+Sinusitis")
        ),
        eventCohorts = list(
          list(1, "Acetaminophen"),
          list(3, "Aspirin"),
          list(6, "Doxylamin")
        ),
        exitCohorts = list(
          list(8, "death")
        ),
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
      ),
      tp$createAnalysisSpecification(
        targetCohorts = list(
          list(8, "ViralSinusitis")
        ),
        eventCohorts = list(
          list(1, "Acetaminophen"),
          list(3, "Aspirin"),
          list(6, "Doxy-lamin"),
          list(7, "Penicil+linV")
        ),
        exitCohorts = list(
          list(8, "death")
        ),
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
    )
  )
})


test_that("TreatmentPatterns: execute method with multiple analysis", {
  skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))

  tempDir <- file.path(tempdir(), "Strategus-TP")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()
  tp <- Strategus::TreatmentPatternsModule$new()

  modSpecs <- list(
    tp$createAnalysisSpecification(
      targetCohorts = list(
        list(8, "ViralSinusitis")
      ),
      eventCohorts = list(
        list(1, "Acetaminophen"),
        list(3, "Aspirin"),
        list(6, "Doxylamin")
      ),
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
    ),
    tp$createAnalysisSpecification(
      targetCohorts = list(
        list(8, "ViralSinusitis")
      ),
      eventCohorts = list(
        list(1, "Acetaminophen"),
        list(7, "PenicillinV")
      ),
      exitCohorts = list(
        list(8, "death")
      ),
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
  )

  specifications <- tp$createMultiAnalysisModuleSpecification(tpAnalysisList = modSpecs)

  analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
    Strategus::addTreatmentPatternsModuleSpecifications(specifications)

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

  Strategus::execute(
    connectionDetails = testSettings$connectionDetails,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )


  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "attrition.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_age.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_sex.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_year.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "metadata.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "summary_event_duration.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "treatment_pathways.csv"))))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "resultsDataModelSpecification.csv")))
  expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "analysis_cohorts.csv"))))
})
