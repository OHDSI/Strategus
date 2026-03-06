library(testthat)
library(Strategus)

test_that("CohortMethodModule - create module settings compatible for CM v6", {
  cmModule <- CohortMethodModule$new()
  expect_error(
    cmModule$createModuleSpecifications(
      cmAnalysisList = list()
    )
  )

  expect_error(
    cmModule$createModuleSpecifications(
      targetComparatorOutcomesList = list()
    )
  )

  expect_error(
    cmModule$createModuleSpecifications(
      refitPsForEveryOutcome = FALSE
    )
  )

  expect_error(
    cmModule$createModuleSpecifications(
      refitPsForEveryStudyPopulation = TRUE
    )
  )

  expect_error(
    cmModule$createModuleSpecifications(
      cmDiagnosticThresholds = list()
    )
  )
})

test_that("Verify CM v5 module specs fails to execute with error", {
  studyRootFolder <- file.path(tempDir, "cmV5SettingsTest")
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

  cmModule <- CohortMethodModule$new()
  cmModuleSpecifications <- cmModule$createModuleSpecifications(
    cmAnalysesSpecifications = list()
  )
  # Hack to remove the CM v6 module setting
  cmModuleSpecifications$settings$cmAnalysesSpecifications <- NULL
  # Hack to add the CM v5 module settings
  cmModuleSpecifications$settings$cmAnalysisList <- list()
  cmModuleSpecifications$settings$targetComparatorOutcomesList <- list()

  analysisSpecifications <- createEmptyAnalysisSpecifications() |>
    addModuleSpecifications(cmModuleSpecifications)

  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "unit_test"),
    workFolder = workFolder,
    resultsFolder = resultsFolder
  )
  expect_error(
    cmModule$execute(
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings,
      connectionDetails = connectionDetails
    )
  )
})
