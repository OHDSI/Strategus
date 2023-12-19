test_that("Run unit test study", {
  # NOTE: Need to set this in each test otherwise it goes out of scope
  renv:::renv_scope_envvars(RENV_PATHS_CACHE = renvCachePath)

  # Setup keyring for the test
  Sys.setenv("STRATEGUS_KEYRING_PASSWORD" = keyringPassword)
  createKeyringForUnitTest(selectedKeyring = keyringName, selectedKeyringPassword = keyringPassword)
  on.exit(deleteKeyringForUnitTest())

  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/unitTestAnalysisSpecification.json",
      package = "Strategus"
    )
  )

  withr::defer(
    {
      if (usingTempDir) {
        unlink(file.path(tempDir, "EunomiaTestStudy"), recursive = TRUE, force = TRUE)
      }
    },
    testthat::teardown_env()
  )

  for (i in 1:length(connectionDetailsList)) {
    connectionDetails <- connectionDetailsList[[i]]$connectionDetails
    dbms <- connectionDetailsList[[i]]$connectionDetails$dbms
    workDatabaseSchema <- connectionDetailsList[[i]]$workDatabaseSchema
    cdmDatabaseSchema <- connectionDetailsList[[i]]$cdmDatabaseSchema
    tempEmulationSchema <- connectionDetailsList[[i]]$tempEmulationSchema
    studyRootFolder <- file.path(tempDir, "EunomiaTestStudy", dbms)
    workFolder <- file.path(studyRootFolder, "work_folder")
    resultsFolder <- file.path(studyRootFolder, "results_folder")
    scriptFolder <- file.path(studyRootFolder, "script_folder")

    message("************* Running Strategus on ", dbms, " *************")

    # Using a named and secured keyring
    Strategus::storeConnectionDetails(
      connectionDetails = connectionDetails,
      connectionDetailsReference = dbms,
      keyringName = keyringName
    )

    resultsConnectionDetailsReference <- NULL
    resultsDatabaseSchema <- NULL

    # Only run this code if we're testing on SQLite
    if (dbms == "sqlite") {
      resultsConnectionDetailsReference <- "result-store"
      resultsDatabaseSchema <- "main"
      Strategus::storeConnectionDetails(
        connectionDetails,
        resultsConnectionDetailsReference,
        keyringName = keyringName
      )
      resultsExecutionSettings <- Strategus::createResultsExecutionSettings(
        resultsConnectionDetailsReference = resultsConnectionDetailsReference,
        resultsDatabaseSchema = resultsDatabaseSchema,
        workFolder = workFolder,
        resultsFolder = resultsFolder
      )
      Strategus::createResultDataModels(
        analysisSpecifications = analysisSpecifications,
        executionSettings = resultsExecutionSettings,
        keyringName = keyringName
      )
    }

    executionSettings <- createCdmExecutionSettings(
      connectionDetailsReference = dbms,
      workDatabaseSchema = workDatabaseSchema,
      cdmDatabaseSchema = cdmDatabaseSchema,
      tempEmulationSchema = tempEmulationSchema,
      cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = paste0("s", tableSuffix)),
      workFolder = workFolder,
      resultsFolder = resultsFolder,
      minCellCount = 5,
      resultsDatabaseSchema = resultsDatabaseSchema,
      resultsConnectionDetailsReference = resultsConnectionDetailsReference
    )

    if (!dir.exists(studyRootFolder)) {
      dir.create(studyRootFolder, recursive = TRUE)
    }
    ParallelLogger::saveSettingsToJson(
      object = executionSettings,
      file.path(studyRootFolder, "eunomiaExecutionSettings.json")
    )

    executionSettings <- ParallelLogger::loadSettingsFromJson(
      fileName = file.path(studyRootFolder, "eunomiaExecutionSettings.json")
    )

    Strategus::execute(
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings,
      executionScriptFolder = scriptFolder,
      keyringName = keyringName
    )

    expect_true(file.exists(file.path(resultsFolder, "TestModule1_1", "done")))
  }
})
