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
  Strategus::uploadResults(
    analysisSpecifications = analysisSpecifications,
    resultsDataModelSettings = resultsDataModelSettings,
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
  resultsDataModelSettings <- Strategus::createResultsDataModelSettings(
    resultsDatabaseSchema = resultsDataModelSettings$resultsDatabaseSchema,
    resultsFolder = resultsExecutionSettings$resultsFolder
  )

  # NOTE: This will throw a warning since the database metadata
  # does not exist
  expect_warning(
    Strategus::uploadResults(
      analysisSpecifications = resultsModulesAnalysisSpecifications,
      resultsDataModelSettings = resultsDataModelSettings,
      resultsConnectionDetails = resultsConnectionDetails
    )
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

test_that("Negative control outcomes are optional", {
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
      package = "Strategus"
    )
  )

  # Remove the nco section
  analysisSpecifications$sharedResources <- list(analysisSpecifications$sharedResources[[1]])

  # Remove all but CG
  analysisSpecifications$moduleSpecifications <- list(analysisSpecifications$moduleSpecifications[[3]])

  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "unit_test"),
    workFolder = file.path(tempDir, "work_folder"),
    resultsFolder = file.path(tempDir, "results_folder")
  )

  expect_output(
    Strategus::execute(
      connectionDetails = connectionDetails,
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings
    ),
    "Generating cohort set",
    ignore.case = TRUE
  )
})

test_that("Specify subset of modules to run with modules not in specification fails", {
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
      package = "Strategus"
    )
  )
  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "unit_test"),
    workFolder = file.path(tempDir, "work_folder"),
    resultsFolder = file.path(tempDir, "results_folder"),
    modulesToExecute = c("foobar")
  )

  expect_error(
    Strategus::execute(
      connectionDetails = connectionDetails,
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings
    )
  )

  executionSettings <- createResultsExecutionSettings(
    resultsDatabaseSchema = "main",
    workFolder = file.path(tempDir, "work_folder"),
    resultsFolder = file.path(tempDir, "results_folder"),
    modulesToExecute = c("foobar")
  )

  expect_error(
    Strategus::execute(
      connectionDetails = connectionDetails,
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings
    )
  )
})

test_that("Specify subset of modules to run", {
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
      package = "Strategus"
    )
  )

  modulesToExecute <- c("CohortGeneratorModule", "CohortIncidenceModule")
  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "unit_test"),
    workFolder = file.path(tempDir, "work_folder"),
    resultsFolder = file.path(tempDir, "results_folder"),
    modulesToExecute = modulesToExecute
  )

  output <- Strategus::execute(
    connectionDetails = connectionDetails,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )

  modulesExecuted <- sapply(
    X = output,
    FUN = function(x) {
      x$moduleName
    }
  )

  expect_true(all(modulesExecuted %in% modulesToExecute))

  # Create a results DB and upload results
  dbFilePath <- file.path(tempDir, "testdm.sqlite")
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
    resultsFolder = executionSettings$resultsFolder,
    modulesToExecute = modulesToExecute
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
  Strategus::uploadResults(
    analysisSpecifications = analysisSpecifications,
    resultsDataModelSettings = resultsDataModelSettings,
    resultsConnectionDetails = resultsConnectionDetails
  )
})

test_that("Stop if error occurs during cohort generation", {
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
      package = "Strategus"
    )
  )
  # Add an ill-formed Circe expression to break the cohort generation process
  analysisSpecifications$sharedResources[[1]]$cohortDefinitions[[6]] <- list(
    cohortId = 6,
    cohortName = "Failure",
    cohortDefinition = "{}"
  )

  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "unit_test"),
    workFolder = file.path(tempDir, "work_folder"),
    resultsFolder = file.path(tempDir, "results_folder")
  )

  output <- Strategus::execute(
    connectionDetails = connectionDetails,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )

  # Verify cohort generator failed
  cohortGeneratorStatus <- sapply(output, function(x) if (x$moduleName == "CohortGeneratorModule") x$status)
  cohortGeneratorStatus <- unlist(cohortGeneratorStatus[-which(sapply(cohortGeneratorStatus, is.null))])
  expect_true(cohortGeneratorStatus == "FAILED")

  # Verify all other modules were skipped
  allOtherModuleStatuses <- sapply(output, function(x) if (x$moduleName != "CohortGeneratorModule") x$status)
  allOtherModuleStatuses <- unlist(allOtherModuleStatuses)
  expect_true(all(allOtherModuleStatuses == "SKIPPED"))
})
