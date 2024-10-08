# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of Strategus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Note: Using S3 for consistency with settings objects in PLP, CohortMethod, and
# FeatureExtraction. If we want to use S4 or R6 we need to first adapt those
# packages. This will be difficult, since these settings objects are used throughout
# these packages, and are for example used in do.call() calls. We should also
# carefully consider serialization and deserialization to JSON, which currently
# uses custom functionality in ParallelLogger to maintain object attributes.

#' Provides the file path to the database identifier file created
#' by Strategus
#'
#' @description
#' This function is used to identify the location of the database identifier
#' created by Strategus when executing an analysis specification. This
#' location is important when uploading results since the database identifier
#' may be needed to purge old results for a given database identifier.
#'
#' @template resultsFolder
#' @noRd
#' @keywords internal
getDatabaseIdentifierFilePath <- function(resultsFolder) {
  return(file.path(.getDatabaseMetaDataResultsFolder(resultsFolder), "database_meta_data.csv"))
}

#' Gets the metadata for your OMOP CDM Database
#'
#' @description
#' This function is used to gather metadata about your OMOP CDM and inspect
#' for informational purposes. This information will be saved with your
#' results when executing an analysis specification.
#'
#' @param cdmExecutionSettings    An object of type `CdmExecutionSettings` as
#'                                created [createCdmExecutionSettings()].
#' @template connectionDetails
#'
#' @export
getCdmDatabaseMetaData <- function(cdmExecutionSettings, connectionDetails) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertClass(cdmExecutionSettings, "CdmExecutionSettings", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Verify the CDM tables required by this function exist prior to
  # querying. Then we can stop the processing and provide an informative
  # message to the user.
  requiredTables <- c("cdm_source", "vocabulary", "observation_period")
  cdmTableList <- DatabaseConnector::getTableNames(
    connection = connection,
    databaseSchema = cdmExecutionSettings$cdmDatabaseSchema
  )
  cdmTableList <- unique(tolower(cdmTableList))

  if (length(cdmTableList) == 0) {
    stop(sprintf("FATAL ERROR: No tables found in your OMOP CDM. Please confirm you are using the proper connection information, in particular the CDM schema name."))
  }

  if (!length(cdmTableList[which(x = cdmTableList %in% requiredTables)]) == length(requiredTables)) {
    missingCdmTables <- requiredTables[!(requiredTables %in% cdmTableList)]
    stop(sprintf("FATAL ERROR: Your OMOP CDM is missing the following required tables: %s", paste(missingCdmTables, collapse = ", ")))
  }

  resultsDataModel <- .getDatabaseMetaDataRdms()

  sql <- "SELECT TOP 1 * FROM @cdm_database_schema.cdm_source;"
  cdmSource <- renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE,
    cdm_database_schema = cdmExecutionSettings$cdmDatabaseSchema
  )

  # Verify that the cdmSource table has information
  if (nrow(cdmSource) == 0) {
    stop("FATAL ERROR: The CDM_SOURCE table in your OMOP CDM is empty. Please populate this table with information about your CDM and organization. For more information, please see: https://ohdsi.github.io/CommonDataModel/cdm53.html#CDM_SOURCE")
  }

  # Restrict the cdmSource columns to those that are
  # expected in the resultsDataModel
  cdmSource <- cdmSource[, which(names(cdmSource) %in% SqlRender::snakeCaseToCamelCase(resultsDataModel$columnName))]

  # In the case that the CDM is pre v5.4, it will lack the new
  # cdm_version_concept_id column. In this case, we'll default it to
  # concept_id == 1147638 which is CDM v5.3.1
  # Ref: https://ohdsi.github.io/CommonDataModel/cdm54Changes.html
  if (!"cdmVersionConceptId" %in% names(cdmSource)) {
    cdmSource$cdmVersionConceptId <- 1147638
  }

  sql <- "SELECT TOP 1 vocabulary_version  FROM @cdm_database_schema.vocabulary WHERE vocabulary_id = 'None';"
  vocabVersion <- renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE,
    cdm_database_schema = cdmExecutionSettings$cdmDatabaseSchema
  )

  # Verify that the vocabulary_version is found
  if (nrow(vocabVersion) == 0) {
    stop("FATAL ERROR: The VOCABULARY table in your OMOP CDM is missing the version. Please verify that your process for loading the vocabulary included an entry in the vocabulary table with vocabulary_id == 'None'")
  }


  sql <- "SELECT MAX(observation_period_end_date) as max_obs_period_end_date
  FROM @cdm_database_schema.observation_period;"
  observationPeriodMax <- renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE,
    cdm_database_schema = cdmExecutionSettings$cdmDatabaseSchema
  )

  # Verify that the MAX(observation_period_end_date) is a valid date
  if (is.na(observationPeriodMax$maxObsPeriodEndDate)) {
    stop("FATAL ERROR: The OBSERVATION_PERIOD table in your OMOP CDM lacks a maximum observation_period_end_date. This may be a result of an error in the ETL as each person in the OMOP CDM must have an observation period with a valid start and end date.")
  }

  databaseId <- digest::digest2int(paste(cdmSource$cdmSourceName, cdmSource$cdmReleaseDate, cdmSource$cdmHolder))
  databaseMetaData <- cdmSource %>%
    mutate(
      vocabularyVersion = vocabVersion$vocabularyVersion,
      databaseId = !!databaseId
    ) %>%
    bind_cols(observationPeriodMax)

  return(databaseMetaData)
}

.writeDatabaseMetaData <- function(databaseMetaData, executionSettings) {
  # Save the results
  databaseMetaDataFolder <- .getDatabaseMetaDataResultsFolder(executionSettings$resultsFolder)
  if (!dir.exists(databaseMetaDataFolder)) {
    dir.create(databaseMetaDataFolder, recursive = TRUE)
  }

  resultsDataModel <- .getDatabaseMetaDataRdms()
  # Export the csv files:
  CohortGenerator::writeCsv(
    x = databaseMetaData,
    file = file.path(databaseMetaDataFolder, "database_meta_data.csv")
  )

  CohortGenerator::writeCsv(
    x = resultsDataModel,
    file = file.path(databaseMetaDataFolder, "resultsDataModelSpecification.csv"),
    warnOnFileNameCaseMismatch = FALSE
  )
}

.createDatabaseMetadataResultsDataModel <- function(resultsConnectionDetails,
                                                    resultsDataModelSettings) {
  rdmsFile <- file.path(.getDatabaseMetaDataResultsFolder(resultsDataModelSettings$resultsFolder), "resultsDataModelSpecification.csv")
  if (file.exists(rdmsFile)) {
    rlang::inform("Creating results data model for database metadata")
    connection <- DatabaseConnector::connect(resultsConnectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))

    # Create the SQL from the resultsDataModelSpecification.csv
    sql <- ResultModelManager::generateSqlSchema(
      csvFilepath = rdmsFile
    )
    sql <- SqlRender::render(
      sql = sql,
      database_schema = resultsDataModelSettings$resultsDatabaseSchema
    )
    DatabaseConnector::executeSql(connection = connection, sql = sql)
  } else {
    warning("DatabaseMetaData not found - skipping table creation")
  }
}

.uploadDatabaseMetadata <- function(resultsConnectionDetails,
                                    resultsDataModelSettings) {
  databaseMetaDataResultsFolder <- .getDatabaseMetaDataResultsFolder(resultsDataModelSettings$resultsFolder)
  rdmsFile <- file.path(.getDatabaseMetaDataResultsFolder(resultsDataModelSettings$resultsFolder), "resultsDataModelSpecification.csv")
  if (file.exists(rdmsFile)) {
    rlang::inform("Uploading database metadata")
    connection <- DatabaseConnector::connect(resultsConnectionDetails)
    on.exit(DatabaseConnector::disconnect(connection))

    specification <- CohortGenerator::readCsv(file = rdmsFile)
    ResultModelManager::uploadResults(
      connection = connection,
      schema = resultsDataModelSettings$resultsDatabaseSchema,
      resultsFolder = databaseMetaDataResultsFolder,
      purgeSiteDataBeforeUploading = TRUE,
      databaseIdentifierFile = getDatabaseIdentifierFilePath(resultsDataModelSettings$resultsFolder),
      specifications = specification
    )
  } else {
    warning("DatabaseMetaData not found - skipping table creation")
  }
}

.getDatabaseMetaDataResultsFolder <- function(resultsFolder) {
  return(file.path(resultsFolder, "DatabaseMetaData"))
}

.getDatabaseMetaDataRdms <- function() {
  resultsDataModel <- CohortGenerator::readCsv(
    file = system.file(
      file.path("csv", "databaseMetaDataRdms.csv"),
      package = "Strategus"
    ),
    warnOnCaseMismatch = FALSE
  )
  invisible(resultsDataModel)
}
