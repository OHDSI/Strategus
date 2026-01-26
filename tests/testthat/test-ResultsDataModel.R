test_that("Create results data model with subset of modules specified emits warning", {
  # Create a results DB and upload results
  dbFilePath <- file.path(tempDir, "EunomiaTestStudy")
  mydb <- dbConnect(RSQLite::SQLite(), dbFilePath)
  dbDisconnect(mydb)

  withr::defer(
    {
      unlink(dbFilePath, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )

  resultsConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = dbFilePath
  )

  resultsDataModelSettings <- Strategus::createResultsDataModelSettings(
    resultsDatabaseSchema = "main",
    resultsFolder = tempDir,
    modulesToExecute = c("CohortGeneratorModule")
  )

  # Create results data model -------------------------
  cdmModulesAnalysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
                           package = "Strategus"
    )
  )

  expect_warning(
    Strategus::createResultDataModel(
      analysisSpecifications = cdmModulesAnalysisSpecifications,
      resultsDataModelSettings = resultsDataModelSettings,
      resultsConnectionDetails = resultsConnectionDetails
    )
  )
})

