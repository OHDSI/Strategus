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

execute <- function(jobContext) {
  rlang::inform("Validating inputs")
  checkmate::assert_list(x = jobContext)
  if (is.null(jobContext$settings)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }

  rlang::inform("Executing")
  # Establish the connection and ensure the cleanup is performed
  connection <- DatabaseConnector::connect(jobContext$moduleExecutionSettings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  sql <- "CREATE TABLE #Codesets (
            codeset_id int NOT NULL,
            concept_id bigint NOT NULL
          )
          ;

          INSERT INTO #Codesets (codeset_id, concept_id)
          SELECT 0 as codeset_id, c.concept_id
          FROM @cdm_database_schema.CONCEPT c
          WHERE c.concept_id = 0
          ;"

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = sql,
    tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema,
    cdm_database_schema = jobContext$moduleExecutionSettings$cdmDatabaseSchema
  )

  data <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = "SELECT * FROM #Codesets;",
    tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema
  )

  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "TRUNCATE TABLE #Codesets; DROP TABLE #Codesets;",
    tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema
  )

  message("Exporting data")
  moduleInfo <- getModuleInfo()
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  fileName <- file.path(resultsFolder, paste0(moduleInfo$TablePrefix, "data.csv"))
  readr::write_csv(data, fileName)

  ParallelLogger::logTrace("Finished TestModule1")
}

# Private methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}
