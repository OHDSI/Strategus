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

test_that("Verify SCCS v5 module specs fails to execute with error", {
  studyRootFolder <- file.path(tempDir, "SccsV5SettingsTest")
  workFolder <- file.path(studyRootFolder, "work_folder")
  resultsFolder <- file.path(studyRootFolder, "results_folder")
  if (!dir.exists(studyRootFolder)) {
    dir.create(studyRootFolder, recursive = TRUE)
  }

  withr::defer(
    {
      unlink(studyRootFolder, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )

  sccsModule <- SelfControlledCaseSeriesModule$new()
  sccsModuleSpecifications <- sccsModule$createModuleSpecifications(
    sccsAnalysesSpecifications = list()
  )
  # Hack to remove the SCCS v6 module setting
  sccsModuleSpecifications$settings$sccsAnalysesSpecifications <- NULL
  # Hack to add the SCCS v5 module settings
  sccsModuleSpecifications$settings$sccsAnalysisList <- list()
  sccsModuleSpecifications$settings$exposuresOutcomeList <- list()

  analysisSpecifications <- createEmptyAnalysisSpecifications() |>
    addModuleSpecifications(sccsModuleSpecifications)

  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "unit_test"),
    workFolder = workFolder,
    resultsFolder = resultsFolder
  )
  expect_error(
    sccsModule$execute(
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings,
      connectionDetails = connectionDetails
    )
  )
})
