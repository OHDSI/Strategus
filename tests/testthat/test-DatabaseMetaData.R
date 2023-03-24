test_that("Test DatabaseMetaData error conditions", {
  skip_if_not(dbms == "sqlite")
  connection <- DatabaseConnector::connect(connectionDetails)
  # Rename all required tables
  requiredTables <- c("cdm_source", "vocabulary", "observation_period")
  renameTableSql <- "ALTER TABLE @table RENAME TO @backup_table;"
  for (i in 1:length(requiredTables)) {
    DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                                 sql = renameTableSql,
                                                 progressBar = FALSE,
                                                 reportOverallTime = FALSE,
                                                 table = requiredTables[i],
                                                 backup_table = paste0(requiredTables[i], "_bak"))
  }

  on.exit(expr = {
    # Put the DB back the way we found it
    restoreTableSql<- "DROP TABLE IF EXISTS @cdm_table; ALTER TABLE @backup_table RENAME TO @cdm_table;"
    for (i in 1:length(requiredTables)) {
      DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                                   sql = restoreTableSql,
                                                   progressBar = FALSE,
                                                   reportOverallTime = FALSE,
                                                   backup_table = paste0(requiredTables[i], "_bak"),
                                                   cdm_table = requiredTables[i])
    }
  })

  # Confirm an error is thrown when 1 or more of these tables are missing
  Strategus::storeConnectionDetails(
    connectionDetails = connectionDetails,
    connectionDetailsReference = dbms
  )
  executionSettings <- Strategus::createCdmExecutionSettings(
    connectionDetailsReference = dbms,
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(),
    workFolder = file.path(tempDir, "EunomiaTestStudy/work_folder"),
    resultsFolder = file.path(tempDir, "EunomiaTestStudy/results_folder"),
    minCellCount = 5
  )
  expect_error(Strategus:::createDatabaseMetaData(executionSettings = executionSettings),
               regexp = "FATAL ERROR: Your OMOP CDM is missing the following required tables: cdm_source, vocabulary, observation_period")

  # Create required tables with no information verify this throws an error
  emptyTableSql = "SELECT * INTO @cdm_table FROM @table WHERE 1 = 0;"
  for (i in 1:length(requiredTables)) {
    DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                                 sql = emptyTableSql,
                                                 progressBar = FALSE,
                                                 reportOverallTime = FALSE,
                                                 table = paste0(requiredTables[i], "_bak"),
                                                 cdm_table = requiredTables[i])
  }

  expect_error(Strategus:::createDatabaseMetaData(executionSettings = executionSettings),
               regexp = "FATAL ERROR: The CDM_SOURCE table in your OMOP CDM is empty.")

  # Populate the CDM_SOURCE table so we can check that the vocabulary check works
  restoreTableSql = "DROP TABLE IF EXISTS @cdm_table; SELECT * INTO @cdm_table FROM @backup_table;"
  DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                               sql = restoreTableSql,
                                               progressBar = FALSE,
                                               reportOverallTime = FALSE,
                                               cdm_table = "cdm_source",
                                               backup_table = "cdm_source_bak")

  expect_error(Strategus:::createDatabaseMetaData(executionSettings = executionSettings),
               regexp = "FATAL ERROR: The VOCABULARY table in your OMOP CDM is missing the version")

  # Populate the VOCABULARY table so we can check that the observation_period check works
  DatabaseConnector::renderTranslateExecuteSql(connection = connection,
                                               sql = restoreTableSql,
                                               progressBar = FALSE,
                                               reportOverallTime = FALSE,
                                               cdm_table = "vocabulary",
                                               backup_table = "vocabulary_bak")
  expect_error(Strategus:::createDatabaseMetaData(executionSettings = executionSettings),
               regexp = "FATAL ERROR: The OBSERVATION_PERIOD table in your OMOP CDM lacks a maximum observation_period_end_date")
})
