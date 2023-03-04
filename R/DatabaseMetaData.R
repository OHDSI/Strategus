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

  # Restrict the cdmSource columns to those that are
  # expected in the resultsDataModel
  cdmSource <- cdmSource[,which(names(cdmSource) %in% SqlRender::snakeCaseToCamelCase(resultsDataModel$columnName))]

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

  sql <- "SELECT MAX(observation_period_end_date) as max_obs_period_end_date
  FROM @cdm_database_schema.observation_period;"
  observationPeriodMax <- renderTranslateQuerySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = TRUE,
    cdm_database_schema = executionSettings$cdmDatabaseSchema
  )

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
