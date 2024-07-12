test_that("Execute study, upload results, excute results modules and upload results", {
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
                           package = "Strategus"
    )
  )
  studyRootFolder <- file.path(tempDir, "EunomiaTestStudy")
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

  # Execute the study ---------------------------
  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "unit_test"),
    workFolder = workFolder,
    resultsFolder = resultsFolder
  )

  ParallelLogger::saveSettingsToJson(
    object = executionSettings,
    file.path(studyRootFolder, "eunomiaExecutionSettings.json")
  )

  executionSettings <- ParallelLogger::loadSettingsFromJson(
    fileName = file.path(studyRootFolder, "eunomiaExecutionSettings.json")
  )

  expect_warning(
    Strategus::execute(
      connectionDetails = connectionDetails,
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings
    )
  )

  # Create a results DB and upload results
  dbFilePath <- file.path(studyRootFolder, "testdm.sqlite")
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
    resultsFolder = executionSettings$resultsFolder
  )

  # Create cdm modules results data model -------------------------
  cdmModulesAnalysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
                           package = "Strategus"
    )
  )

  Strategus::createResultDataModel(
    analysisSpecifications = cdmModulesAnalysisSpecifications,
    resultsDataModelSettings = resultsDataModelSettings,
    resultsConnectionDetails = resultsConnectionDetails
  )

  # Upload cdm related results --------------------
  resultsUploadSettings <- Strategus::createResultsUploadSettings(
    resultsDatabaseSchema = resultsDataModelSettings$resultsDatabaseSchema,
    resultsFolder = resultsDataModelSettings$resultsFolder
  )

  Strategus::uploadResults(
    analysisSpecifications = analysisSpecifications,
    resultsUploadSettings = resultsUploadSettings,
    resultsConnectionDetails = resultsConnectionDetails
  )

  # Execute results modules -------------------------
  resultsModulesAnalysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/resultsModulesAnalysisSpecifications.json",
                           package = "Strategus"
    )
  )

  resultsExecutionSettings <- Strategus::createResultsExecutionSettings(
    resultsDatabaseSchema = resultsDataModelSettings$resultsDatabaseSchema,
    workFolder = file.path(studyRootFolder, "results_modules", "work_folder"),
    resultsFolder = file.path(studyRootFolder, "results_modules", "results_folder")
  )

  Strategus::execute(
    connectionDetails = resultsConnectionDetails,
    analysisSpecifications = resultsModulesAnalysisSpecifications,
    executionSettings = resultsExecutionSettings
  )

  # Create the results data model ------
  resultsDataModelSettings <- Strategus::createResultsDataModelSettings(
    resultsDatabaseSchema = "main",
    resultsFolder = resultsExecutionSettings$resultsFolder
  )

  # NOTE: This will throw a warning since the database metadata
  # does not exist
  expect_warning(
    Strategus::createResultDataModel(
      analysisSpecifications = resultsModulesAnalysisSpecifications,
      resultsDataModelSettings = resultsDataModelSettings,
      resultsConnectionDetails = resultsConnectionDetails
    )
  )

  # Upload the results -------------
  resultsUploadSettings <- Strategus::createResultsUploadSettings(
    resultsDatabaseSchema = resultsDataModelSettings$resultsDatabaseSchema,
    resultsFolder = resultsExecutionSettings$resultsFolder
  )

  Strategus::uploadResults(
    analysisSpecifications = resultsModulesAnalysisSpecifications,
    resultsUploadSettings = resultsUploadSettings,
    resultsConnectionDetails = resultsConnectionDetails

  )

  # Get a list of tables
  connection <- DatabaseConnector::connect(resultsConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  tableList <- DatabaseConnector::getTableNames(
    connection = connection,
    databaseSchema = resultsDataModelSettings$resultsDatabaseSchema
  )

  expect_true(length(tableList) > 0)
})

test_that("Execute on Oracle stops if table names exceed length limit", {
  sqlRenderTempEmulationSchema <- getOption("sqlRenderTempEmulationSchema", default = "")
  options(sqlRenderTempEmulationSchema = "some_schema")
  on.exit(options(sqlRenderTempEmulationSchema = sqlRenderTempEmulationSchema))

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "oracle"
  )
  executionSettings <- Strategus::createCdmExecutionSettings(
    workDatabaseSchema = "does_not_matter",
    cdmDatabaseSchema = "does_not_matter",
    cohortTableNames = CohortGenerator::getCohortTableNames("some_really_long_table_name_for_testing_that_oracle_throws_a_warning"),
    workFolder = file.path(tempDir, "work_folder"),
    resultsFolder = file.path(tempDir, "results_folder"),
    minCellCount = 5
  )

  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
                           package = "Strategus"
    )
  )

  expect_error(
    Strategus::execute(
      connectionDetails = connectionDetails,
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings
    )
  )
})
