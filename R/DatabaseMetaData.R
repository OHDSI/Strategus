# Copyright 2023 Observational Health Data Sciences and Informatics
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

createDatabaseMetaData <- function(executionSettings, keyringName = NULL) {
  databaseMetaDataFolder <- file.path(executionSettings$resultsFolder, "DatabaseMetaData")
  if (!dir.exists(databaseMetaDataFolder)) {
    dir.create(databaseMetaDataFolder, recursive = TRUE)
  }

  connectionDetails <- retrieveConnectionDetails(
    connectionDetailsReference = executionSettings$connectionDetailsReference,
    keyringName = keyringName
  )
  connection <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Verify the CDM tables required by this function exist prior to
  # querying. Then we can stop the processing and provide an informative
  # message to the user.
  requiredTables <- c("cdm_source", "vocabulary", "observation_period")
  cdmTableList <- DatabaseConnector::getTableNames(
    connection = connection,
    databaseSchema = executionSettings$cdmDatabaseSchema
  )
  cdmTableList <- unique(tolower(cdmTableList))

  if (!length(cdmTableList[which(x = cdmTableList %in% requiredTables)]) == length(requiredTables)) {
    missingCdmTables <- requiredTables[!(requiredTables %in% cdmTableList)]
    stop(sprintf("FATAL ERROR: Your OMOP CDM is missing the following required tables: %s", paste(missingCdmTables, collapse = ", ")))
  }

  resultsDataModel <- CohortGenerator::readCsv(
    file = system.file("databaseMetaDataRdms.csv", package = "Strategus"),
    warnOnCaseMismatch = FALSE
  )

  sql <- "SELECT TOP 1 * FROM @cdm_database_schema.cdm_source;"
  cdmSource <- renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE,
    cdm_database_schema = executionSettings$cdmDatabaseSchema
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
    cdm_database_schema = executionSettings$cdmDatabaseSchema
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
    cdm_database_schema = executionSettings$cdmDatabaseSchema
  )

  # Verify that the MAX(observation_period_end_date) is a valid date
  if (is.na(observationPeriodMax$maxObsPeriodEndDate)) {
    stop("FATAL ERROR: The OBSERVATION_PERIOD table in your OMOP CDM lacks a maximum observation_period_end_date. This may be a result of an error in the ETL as each person in the OMOP CDM must have an observation period with a valid start and end date.")
  }

  databaseId <- digest::digest2int(paste(cdmSource$cdmSourceName, cdmSource$cdmReleaseDate))
  database <- cdmSource %>%
    mutate(
      vocabularyVersion = vocabVersion$vocabularyVersion,
      databaseId = !!databaseId
    ) %>%
    bind_cols(observationPeriodMax)

  # Export the csv files:
  CohortGenerator::writeCsv(
    x = database,
    file = file.path(databaseMetaDataFolder, "database_meta_data.csv")
  )

  CohortGenerator::writeCsv(
    x = resultsDataModel,
    file = file.path(databaseMetaDataFolder, "resultsDataModelSpecification.csv"),
    warnOnFileNameCaseMismatch = FALSE
  )
  return(databaseId)
}
