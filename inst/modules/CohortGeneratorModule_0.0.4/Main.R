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
validate <- function(jobContext) {
  logStatus("Validate")
  # Verify the job context details - this feels like a task to centralize for
  # all modules
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
  
  # Validate that the analysis specification will work when we 
  # enter the execute statement. Bad thing here: we're doing
  # double work to construct the cohort definition set but I'm
  # unsure if validate() should potentially change the jobContext
  # to add any necessary elements to the executionSettings list?
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(sharedResources = jobContext$sharedResources,
                                                                 settings = jobContext$settings)
  invisible(cohortDefinitionSet)
}

execute <- function(jobContext) {
  logStatus("Execute")
  # Establish the connection and ensure the cleanup is performed
  connection <- DatabaseConnector::connect(jobContext$moduleExecutionSettings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  # Create the cohort definition set
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(sharedResources = jobContext$sharedResources,
                                                                 settings = jobContext$settings)
  
  # Create the cohort tables
  CohortGenerator::createCohortTables(connection = connection,
                                      cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
                                      cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
                                      incremental = jobContext$settings$incremental)
  # Generate the cohorts
  cohortsGenerated <- CohortGenerator::generateCohortSet(connection = connection,
                                                         cohortDefinitionSet = cohortDefinitionSet,
                                                         cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
                                                         cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
                                                         cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
                                                         incremental = jobContext$settings$incremental,
                                                         incrementalFolder = jobContext$moduleExecutionSettings$workSubFolder)
  
  # Save the generation information
  if (nrow(cohortsGenerated) > 0) {
    cohortsGenerated$databaseId <- jobContext$moduleExecutionSettings$connectionDetailsReference
    # Remove any cohorts that were skipped
    cohortsGenerated <- cohortsGenerated[toupper(cohortsGenerated$generationStatus) != 'SKIPPED',]
  }
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  if (!dir.exists(resultsFolder)) {
    dir.create(resultsFolder, recursive = TRUE)
  }
  CohortGenerator::saveIncremental(data = cohortsGenerated,
                                   fileName = file.path(resultsFolder, "cohortGenerationStats.csv"), 
                                   cohortId = cohortsGenerated$cohortId)
  
  logStatus("Export data")
  # Export the stats table and cohort counts
  CohortGenerator::exportCohortStatsTables(connection = connection,
                                           cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
                                           cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
                                           cohortStatisticsFolder = jobContext$moduleExecutionSettings$resultsSubFolder,
                                           incremental = jobContext$settings$incremental,
                                           databaseId = jobContext$moduleExecutionSettings$connectionDetailsReference)
  
  cohortCounts <- CohortGenerator::getCohortCounts(connection = connection,
                                                   cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
                                                   cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
                                                   databaseId = jobContext$moduleExecutionSettings$connectionDetailsReference)
  
  CohortGenerator::saveIncremental(data = cohortCounts, 
                                   fileName = file.path(jobContext$moduleExecutionSettings$resultsSubFolder, "cohortCounts.csv"), 
                                   cohortId = cohortCounts$cohortId)
}

exportResults <- function(jobContext) {
  return(NULL)
}

importResults <- function(jobContext) {
  return(NULL)
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
      break;
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
    cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(cohortId = as.integer(cohortDefinitions[[i]]$cohortId),
                                                                 cohortName = cohortDefinitions[[i]]$cohortName, 
                                                                 sql = cohortSql,
                                                                 cohortJson = cohortJson,
                                                                 stringsAsFactors = FALSE))    
  }
  return(cohortDefinitionSet)
}

logStatus <- function(status) {
  metaData <- getModuleInfo()
  message <- paste0(status, " - ", metaData$Name, " (v", metaData$Version, ")")
  ParallelLogger::logInfo(paste0(rep("-", nchar(message))))
  ParallelLogger::logInfo(message)
  ParallelLogger::logInfo(paste0(rep("-", nchar(message))))
}
