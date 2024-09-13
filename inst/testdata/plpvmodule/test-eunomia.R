library(testthat)
library(Eunomia)
connectionDetails <- getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails = connectionDetails)

workFolder <- tempfile("work")
dir.create(workFolder)
resultsfolder <- tempfile("results")
dir.create(resultsfolder)
jobContext <- readRDS("tests/testJobContext.rds")
jobContext$moduleExecutionSettings$workSubFolder <- workFolder
jobContext$moduleExecutionSettings$resultsSubFolder <- resultsfolder
jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails

# add model to folder
plpModel <- readRDS("tests/plpModel.rds")
upperWorkDir <- dirname(workFolder)
dir.create(file.path(upperWorkDir,'ModelTransferModule_1'))
modelTransferFolder <- sort(dir(upperWorkDir, pattern = 'ModelTransferModule'), decreasing = T)[1]
modelSaveLocation <- file.path( upperWorkDir, modelTransferFolder, 'models') # hack to use work folder for model transfer 
PatientLevelPrediction::savePlpModel(
  plpModel,
  file.path(modelSaveLocation, 'model_1_1')
  )

test_that("Run module", {
  source("Main.R")
  execute(jobContext)
  resultsFiles <- list.files(resultsfolder)
  expect_true("performances.csv" %in% resultsFiles)
})

unlink(workFolder)
unlink(resultsfolder)
unlink(connectionDetails$server())
