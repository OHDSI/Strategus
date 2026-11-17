library(testthat)
test_that("Test zip file for sharing results", {
  testDir <- tempfile()
  dir.create(testDir)
  on.exit(unlink(testDir, recursive = TRUE))
  writeLines(
    text = "test 1 2 3",
    file.path(testDir, "log.txt")
  )
  dir.create(file.path(testDir, "test"))
  writeLines(
    text = "csv",
    file.path(testDir, "test", "test.csv")
  )

  zipResults(
    resultsFolder = testDir,
    zipFile = file.path(testDir, "test.zip")
  )

  expect_true(file.exists(file.path(testDir, "test.zip")))

  # Verify that only the csv and the execution summary were zipped
  zipFiles <- utils::unzip(
    zipfile = file.path(testDir, "test.zip"),
    list = TRUE
  )
  expect_setequal(zipFiles$Name, c(file.path("test", "test.csv"), "execution_summary.json"))

  # The summary is only written for the zip file
  expect_false(file.exists(file.path(testDir, "execution_summary.json")))
})

test_that("zipResults includes an execution summary without error messages", {
  analysisSpecifications <- createEmptyAnalysisSpecifications()
  analysisSpecifications <- addCohortGeneratorModuleSpecifications(
    analysisSpecifications = analysisSpecifications,
    moduleSpecifications = CohortGeneratorModule$new()$createModuleSpecifications()
  )
  resultsFolder <- tempfile("share-results-")
  on.exit(unlink(resultsFolder, recursive = TRUE), add = TRUE)
  moduleFolder <- file.path(resultsFolder, "CohortGeneratorModule")
  dir.create(moduleFolder, recursive = TRUE)
  writeLines("a,b\n1,2", file.path(moduleFolder, "cohort.csv"))

  taskInformation <- Strategus:::.createTaskInformation(analysisSpecifications, "CohortGeneratorModule")
  operationInformation <- Strategus:::.createOperationInformation("EXECUTION", taskInformation)
  Strategus:::.writeOperationStatus(
    recordPath = Strategus:::.operationStatusPath(resultsFolder, "CohortGeneratorModule", "EXECUTION"),
    operationInformation = operationInformation,
    state = "COMPLETED",
    startTime = Sys.time(),
    endTime = Sys.time(),
    elapsedSeconds = 1,
    outputFiles = Strategus:::.computeOutputFiles(resultsFolder, "CohortGeneratorModule")
  )
  failedFolder <- file.path(resultsFolder, "CohortMethodModule")
  dir.create(failedFolder)
  failedInformation <- operationInformation
  failedInformation$moduleName <- "CohortMethodModule"
  Strategus:::.writeOperationStatus(
    recordPath = file.path(failedFolder, "execution_status.json"),
    operationInformation = failedInformation,
    state = "FAILED",
    startTime = Sys.time(),
    endTime = Sys.time(),
    elapsedSeconds = 1,
    error = simpleError("could not connect to secret-server.example.org")
  )
  invalidFolder <- file.path(resultsFolder, "BrokenModule")
  dir.create(invalidFolder)
  writeLines("not json", file.path(invalidFolder, "execution_status.json"))

  zipFile <- tempfile(fileext = ".zip")
  on.exit(unlink(zipFile), add = TRUE)
  zipResults(resultsFolder = resultsFolder, zipFile = zipFile)

  zipFiles <- utils::unzip(zipfile = zipFile, list = TRUE)$Name
  expect_setequal(zipFiles, c("CohortGeneratorModule/cohort.csv", "execution_summary.json"))

  unzipFolder <- tempfile("unzipped-")
  on.exit(unlink(unzipFolder, recursive = TRUE), add = TRUE)
  utils::unzip(zipFile, exdir = unzipFolder)
  summaryJson <- readLines(file.path(unzipFolder, "execution_summary.json"))
  expect_false(any(grepl("secret-server", summaryJson, fixed = TRUE)))

  summary <- ParallelLogger::loadSettingsFromJson(file.path(unzipFolder, "execution_summary.json"))
  expect_equal(summary$schemaVersion, 1)
  expect_identical(summary$outputFileChecksumAlgorithm, "sha256")
  modules <- summary$modules
  names(modules) <- vapply(modules, `[[`, character(1), "moduleName")
  expect_setequal(names(modules), c("BrokenModule", "CohortGeneratorModule", "CohortMethodModule"))

  expect_identical(modules$BrokenModule$state, "INVALID")
  expect_identical(modules$CohortMethodModule$state, "FAILED")
  expect_identical(modules$CohortMethodModule$errorClass, "simpleError")
  expect_null(modules$CohortMethodModule$errorMessage)

  completed <- modules$CohortGeneratorModule
  expect_identical(completed$state, "COMPLETED")
  expect_identical(completed$taskChecksum, taskInformation$taskChecksum)
  expect_identical(completed$outputFiles[[1]]$path, "CohortGeneratorModule/cohort.csv")
  expect_identical(
    completed$outputFiles[[1]]$sha256,
    digest::digest(file.path(unzipFolder, "CohortGeneratorModule", "cohort.csv"), algo = "sha256", file = TRUE)
  )
})
