createExecutionStatusTestSpecifications <- function() {
  analysisSpecifications <- createEmptyAnalysisSpecifications()
  addCohortGeneratorModuleSpecifications(
    analysisSpecifications = analysisSpecifications,
    moduleSpecifications = CohortGeneratorModule$new()$createModuleSpecifications(generateStats = TRUE)
  )
}

test_that("HADES module registry covers modules and capabilities", {
  registry <- Strategus:::.getHadesModuleRegistry()
  moduleFiles <- list.files(
    testthat::test_path("..", "..", "R"),
    pattern = "^Module-.*\\.R$"
  )
  expectedModules <- paste0(sub("\\.R$", "", sub("^Module-", "", moduleFiles)), "Module")

  expect_named(registry, c("module", "package", "createResultsDataModel"))
  expect_false(anyDuplicated(tolower(registry$module)) > 0)
  expect_setequal(registry$module, expectedModules)
  expect_true(all(nzchar(registry$package)))
  expect_type(registry$createResultsDataModel, "logical")

  validationRow <- registry[registry$module == "PatientLevelPredictionValidationModule", ]
  expect_identical(validationRow$package, "PatientLevelPrediction")
  expect_false(validationRow$createResultsDataModel)
})

test_that("operation checksums use stable CohortGenerator-compatible JSON", {
  expect_identical(
    Strategus:::.computeOperationChecksum(list(b = 2, a = 1)),
    "f98780f0c5b067bee21ba745d900e62e"
  )
  expect_identical(
    Strategus:::.computeOperationChecksum(list(a = 1, b = 2)),
    Strategus:::.computeOperationChecksum(list(b = 2, a = 1))
  )
})

test_that("getExecutionStatus reports missing and matching records", {
  analysisSpecifications <- createExecutionStatusTestSpecifications()
  resultsFolder <- tempfile("execution-status-")
  taskInformation <- Strategus:::.createTaskInformation(
    analysisSpecifications,
    "CohortGeneratorModule"
  )
  operationInformation <- Strategus:::.createOperationInformation("EXECUTION", taskInformation)
  recordPath <- Strategus:::.operationStatusPath(resultsFolder, "CohortGeneratorModule", "EXECUTION")

  status <- getExecutionStatus(analysisSpecifications, resultsFolder)
  expect_s3_class(status, "OperationStatus")
  expect_identical(status$status, "INCOMPLETE")
  expect_identical(status$modules$state, "NOT_STARTED")

  startTime <- Sys.time()
  Strategus:::.writeOperationStatus(recordPath, operationInformation, "RUNNING", startTime)
  status <- getExecutionStatus(analysisSpecifications, resultsFolder)
  expect_identical(status$status, "RUNNING")
  expect_true(status$modules$checksumMatches)

  Strategus:::.writeOperationStatus(
    recordPath,
    operationInformation,
    "COMPLETED",
    startTime,
    Sys.time(),
    elapsedSeconds = 1
  )
  status <- getExecutionStatus(analysisSpecifications, resultsFolder)
  expect_identical(status$status, "COMPLETED")
  expect_identical(status$modules$state, "COMPLETED")
})

test_that("getExecutionStatus distinguishes stale, failed, and invalid records", {
  analysisSpecifications <- createExecutionStatusTestSpecifications()
  resultsFolder <- tempfile("execution-status-")
  taskInformation <- Strategus:::.createTaskInformation(
    analysisSpecifications,
    "CohortGeneratorModule"
  )
  operationInformation <- Strategus:::.createOperationInformation("EXECUTION", taskInformation)
  recordPath <- Strategus:::.operationStatusPath(resultsFolder, "CohortGeneratorModule", "EXECUTION")
  startTime <- Sys.time()

  Strategus:::.writeOperationStatus(
    recordPath,
    operationInformation,
    "FAILED",
    startTime,
    Sys.time(),
    elapsedSeconds = 1,
    error = simpleError("safe diagnostic")
  )
  status <- getExecutionStatus(analysisSpecifications, resultsFolder)
  expect_identical(status$status, "FAILED")
  expect_identical(status$modules$errorMessage, "safe diagnostic")

  changedSpecifications <- analysisSpecifications
  changedSpecifications$moduleSpecifications[[1]]$settings$generateStats <- FALSE
  status <- getExecutionStatus(changedSpecifications, resultsFolder)
  expect_identical(status$status, "INCOMPLETE")
  expect_identical(status$modules$state, "STALE")
  expect_false(status$modules$checksumMatches)

  writeLines("not json", recordPath)
  status <- getExecutionStatus(analysisSpecifications, resultsFolder)
  expect_identical(status$modules$state, "INVALID")
})

test_that("getExecutionStatus validates and filters module names case-insensitively", {
  analysisSpecifications <- createExecutionStatusTestSpecifications()
  resultsFolder <- tempfile("execution-status-")

  status <- getExecutionStatus(
    analysisSpecifications,
    resultsFolder,
    modules = "cohortgeneratormodule"
  )
  expect_identical(status$modules$moduleName, "CohortGeneratorModule")
  expect_error(
    getExecutionStatus(analysisSpecifications, resultsFolder, modules = "UnknownModule"),
    "Unknown modules"
  )
})

test_that("execution settings default completed-task reuse to false", {
  settings <- createResultsExecutionSettings(
    resultsDatabaseSchema = "results",
    workFolder = tempfile("work-"),
    resultsFolder = tempfile("results-")
  )
  expect_false(settings$skipCompletedTasks)
  expect_error(
    createResultsExecutionSettings(
      resultsDatabaseSchema = "results",
      workFolder = tempfile("work-"),
      resultsFolder = tempfile("results-"),
      skipCompletedTasks = NA
    )
  )
})

test_that("results settings default completed-upload reuse to false", {
  settings <- createResultsDataModelSettings(
    resultsDatabaseSchema = "results",
    resultsFolder = tempfile("results-")
  )
  expect_false(settings$skipCompletedUploads)
  expect_error(
    createResultsDataModelSettings(
      resultsDatabaseSchema = "results",
      resultsFolder = tempfile("results-"),
      skipCompletedUploads = NA
    )
  )
})

test_that("upload status is destination specific", {
  analysisSpecifications <- createExecutionStatusTestSpecifications()
  resultsFolder <- tempfile("upload-status-")
  resultsSettings <- createResultsDataModelSettings("results", resultsFolder)
  connectionDetails <- DatabaseConnector::createConnectionDetails("sqlite", server = "first.sqlite")
  otherConnectionDetails <- DatabaseConnector::createConnectionDetails("sqlite", server = "second.sqlite")
  taskInformation <- Strategus:::.createTaskInformation(analysisSpecifications, "CohortGeneratorModule")
  operationInformation <- Strategus:::.createOperationInformation(
    "UPLOAD", taskInformation, resultsSettings, connectionDetails
  )
  recordPath <- Strategus:::.operationStatusPath(resultsFolder, "CohortGeneratorModule", "UPLOAD")
  Strategus:::.writeOperationStatus(
    recordPath, operationInformation, "COMPLETED", Sys.time(), Sys.time(), 0
  )

  status <- getUploadStatus(analysisSpecifications, resultsSettings, connectionDetails)
  expect_s3_class(status, "OperationStatus")
  expect_identical(status$status, "COMPLETED")
  expect_true(endsWith(status$modules$recordPath, "upload_status.json"))

  otherStatus <- getUploadStatus(analysisSpecifications, resultsSettings, otherConnectionDetails)
  expect_identical(otherStatus$status, "INCOMPLETE")
  expect_identical(otherStatus$modules$state, "STALE")
})

test_that("module execution persists state and optionally reuses completion", {
  analysisSpecifications <- createExecutionStatusTestSpecifications()
  resultsFolder <- tempfile("execution-status-")
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  fakeModule <- R6::R6Class(
    "CohortGeneratorModule",
    public = list(
      execute = function(connectionDetails, analysisSpecifications, executionSettings) {
        calls$count <- calls$count + 1L
        invisible(NULL)
      }
    )
  )
  testthat::local_mocked_bindings(CohortGeneratorModule = fakeModule, .package = "Strategus")
  executionSettings <- structure(
    list(resultsFolder = resultsFolder, skipCompletedTasks = FALSE),
    class = "ExecutionSettings"
  )

  firstResult <- Strategus:::.executeModule(
    moduleName = "CohortGeneratorModule",
    connectionDetails = NULL,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )
  expect_identical(firstResult[[1]]$status, "SUCCESS")
  expect_identical(calls$count, 1L)
  expect_identical(
    getExecutionStatus(analysisSpecifications, resultsFolder)$modules$state,
    "COMPLETED"
  )

  executionSettings$skipCompletedTasks <- TRUE
  secondResult <- Strategus:::.executeModule(
    moduleName = "CohortGeneratorModule",
    connectionDetails = NULL,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )
  expect_identical(secondResult[[1]]$status, "SKIPPED")
  expect_identical(calls$count, 1L)
})

test_that("module failures persist sanitized failure state", {
  analysisSpecifications <- createExecutionStatusTestSpecifications()
  resultsFolder <- tempfile("execution-status-")
  fakeModule <- R6::R6Class(
    "CohortGeneratorModule",
    public = list(
      execute = function(connectionDetails, analysisSpecifications, executionSettings) {
        stop("expected failure")
      }
    )
  )
  testthat::local_mocked_bindings(CohortGeneratorModule = fakeModule, .package = "Strategus")
  executionSettings <- structure(
    list(resultsFolder = resultsFolder, skipCompletedTasks = FALSE),
    class = "ExecutionSettings"
  )

  result <- Strategus:::.executeModule(
    moduleName = "CohortGeneratorModule",
    connectionDetails = NULL,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )
  status <- getExecutionStatus(analysisSpecifications, resultsFolder)
  expect_identical(result[[1]]$status, "FAILED")
  expect_identical(status$modules$state, "FAILED")
  expect_identical(status$modules$errorClass, "simpleError")
  expect_identical(status$modules$errorMessage, "expected failure")
})

test_that("module upload persists status and optionally reuses completion", {
  analysisSpecifications <- createExecutionStatusTestSpecifications()
  resultsFolder <- tempfile("upload-status-")
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  fakeModule <- R6::R6Class(
    "CohortGeneratorModule",
    public = list(
      uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
        calls$count <- calls$count + 1L
        invisible(NULL)
      }
    )
  )
  testthat::local_mocked_bindings(CohortGeneratorModule = fakeModule, .package = "Strategus")
  resultsSettings <- createResultsDataModelSettings("results", resultsFolder)
  connectionDetails <- DatabaseConnector::createConnectionDetails("sqlite", server = "results.sqlite")

  firstResult <- Strategus:::.resultDataModelModuleExecution(
    moduleName = "CohortGeneratorModule",
    functionName = "uploadResults",
    resultsConnectionDetails = connectionDetails,
    resultsDataModelSettings = resultsSettings,
    analysisSpecifications = analysisSpecifications
  )
  expect_identical(firstResult[[1]]$status, "SUCCESS")
  expect_identical(calls$count, 1L)
  expect_identical(
    getUploadStatus(analysisSpecifications, resultsSettings, connectionDetails)$status,
    "COMPLETED"
  )

  resultsSettings$skipCompletedUploads <- TRUE
  secondResult <- Strategus:::.resultDataModelModuleExecution(
    moduleName = "CohortGeneratorModule",
    functionName = "uploadResults",
    resultsConnectionDetails = connectionDetails,
    resultsDataModelSettings = resultsSettings,
    analysisSpecifications = analysisSpecifications
  )
  expect_identical(secondResult[[1]]$status, "SKIPPED")
  expect_identical(calls$count, 1L)
})
