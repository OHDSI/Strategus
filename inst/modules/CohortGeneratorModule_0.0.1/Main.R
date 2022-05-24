# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of CohortGeneratorModule
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

# Module methods -------------------------
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

  # Create the cohort definition set
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(
    sharedResources = jobContext$sharedResources,
    settings = jobContext$settings
  )

  rlang::inform("Executing")
  # Establish the connection and ensure the cleanup is performed
  connection <- DatabaseConnector::connect(jobContext$moduleExecutionSettings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))


  # Create the cohort tables
  CohortGenerator::createCohortTables(
    connection = connection,
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
    incremental = jobContext$settings$incremental
  )
  
  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(
    connection = connection,
    cohortDefinitionSet = cohortDefinitionSet,
    cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
    incremental = jobContext$settings$incremental,
    incrementalFolder = jobContext$moduleExecutionSettings$workSubFolder
  )

  # Save the generation information
  if (nrow(cohortsGenerated) > 0) {
    cohortsGenerated$databaseId <- jobContext$moduleExecutionSettings$connectionDetailsReference
    # Remove any cohorts that were skipped
    cohortsGenerated <- cohortsGenerated[toupper(cohortsGenerated$generationStatus) != "SKIPPED", ]
  }

  # Export the results
  rlang::inform("Export data")
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  if (!dir.exists(resultsFolder)) {
    dir.create(resultsFolder, recursive = TRUE)
  }

  CohortGenerator::saveIncremental(
    data = cohortsGenerated,
    fileName = file.path(resultsFolder, "cohort_generation.csv"),
    cohortId = cohortsGenerated$cohortId
  )

  cohortCounts <- CohortGenerator::getCohortCounts(
    connection = connection,
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
    databaseId = jobContext$moduleExecutionSettings$connectionDetailsReference
  )

  CohortGenerator::writeCsv(
    x = cohortCounts,
    file = file.path(resultsFolder, "cohort_count.csv")
  )

  CohortGenerator::exportCohortStatsTables(
    connection = connection,
    cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cohortStatisticsFolder = resultsFolder,
    snakeCaseToCamelCase = FALSE,
    fileNamesInSnakeCase = TRUE,
    incremental = jobContext$settings$incremental,
    databaseId = jobContext$moduleExecutionSettings$connectionDetailsReference
  )

  # Set the table names in resultsDataModelSpecification.csv
  moduleInfo <- getModuleInfo()
  resultsDataModel <- CohortGenerator::readCsv(file = "resultsDataModelSpecification.csv",
                                               warnOnCaseMismatch = FALSE)
  newTableNames <- paste0(moduleInfo$TablePrefix, resultsDataModel$tableName)
  resultsDataModel$tableName <- newTableNames
  CohortGenerator::writeCsv(x = resultsDataModel,
                            file.path(resultsFolder, "resultsDataModelSpecification.csv"),
                            warnOnCaseMismatch = FALSE,
                            warnOnUploadRuleViolations = FALSE)

  # Zip the results
  zipFile <- file.path(resultsFolder, "cohortGeneratorResults.zip")
  resultFiles <- list.files(resultsFolder,
    pattern = ".*\\.csv$"
  )
  oldWd <- setwd(resultsFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(
    zipFile = zipFile,
    files = resultFiles
  )
  rlang::inform(paste("Results available at:", zipFile))
}


# Private methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

createCohortDefinitionSetFromJobContext <- function(sharedResources, settings) {
  cohortDefinitions <- list()
  if (length(sharedResources) <= 0) {
    stop("No shared resources found")
  }
  for (i in 1:length(sharedResources)) {
    if (which(class(sharedResources[[i]]) %in% "CohortDefinitionSharedResources") > 0) {
      cohortDefinitions <- sharedResources[[i]]$cohortDefinitions
      break
    }
  }
  if (length(cohortDefinitions) <= 0) {
    stop("No cohort definitions found")
  }
  cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in 1:length(cohortDefinitions)) {
    cohortJson <- cohortDefinitions[[i]]$cohortDefinition
    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
    cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = settings$generateStats))
    cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(
      cohortId = as.integer(cohortDefinitions[[i]]$cohortId),
      cohortName = cohortDefinitions[[i]]$cohortName,
      sql = cohortSql,
      json = cohortJson,
      stringsAsFactors = FALSE
    ))
  }
  return(cohortDefinitionSet)
}
