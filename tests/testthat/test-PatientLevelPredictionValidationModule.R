library(testthat)
library(Strategus)

test_that("Test PLP Validation Module", {
  workFolder <- tempfile("work")
  dir.create(workFolder)
  resultsFolder <- tempfile("results")
  dir.create(resultsFolder)
  withr::defer(
    {
      unlink(workFolder, recursive = TRUE, force = TRUE)
      unlink(resultsFolder, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )

  # jobContext <- readRDS("tests/testJobContext.rds")
  # jobContext$moduleExecutionSettings$workSubFolder <- workFolder
  # jobContext$moduleExecutionSettings$resultsSubFolder <- resultsFolder
  # jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails

  # add model to folder
  plpModel <-  readRDS(system.file("testdata/plpvmodule/plpModel.rds", package = "Strategus")) #readRDS("tests/plpModel.rds")
  dir.create(file.path(workFolder,'ModelTransferModule_1'))
  modelTransferFolder <- sort(dir(workFolder, pattern = 'ModelTransferModule'), decreasing = T)[1]
  modelSaveLocation <- file.path(workFolder, modelTransferFolder, 'models') # hack to use work folder for model transfer
  PatientLevelPrediction::savePlpModel(
    plpModel,
    file.path(modelSaveLocation, 'model_1_1')
  )

  createCohorts(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDatabaseSchema,
                cohortDatabaseSchema = workDatabaseSchema)

  # Create the validation settings and run the module
  plpvSettingsCreator <- PatientLevelPredictionValidationModule$new()
  plpModuleSettings <- plpvSettingsCreator$createModuleSpecifications()

  analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
    addModuleSpecifications(plpModuleSettings)

  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
    workFolder = workFolder,
    resultsFolder = resultsFolder
  )

  # debugonce(PatientLevelPredictionValidationModule$debug('execute'))
  Strategus::execute(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    connectionDetails = connectionDetails
  )
})
