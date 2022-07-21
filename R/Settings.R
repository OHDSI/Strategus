# Copyright 2022 Observational Health Data Sciences and Informatics
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

#' Create an emtpy analysis specifications object.
#'
#' @return
#' An object of type `AnalysisSpecifications`.
#'
#' @export
createEmptyAnalysisSpecificiations <- function() {
  analysisSpecifications <- list(sharedResources = list(),
                                 moduleSpecifications = list())
  class(analysisSpecifications) <- "AnalysisSpecifications"
  return(analysisSpecifications)
}

#' Add shared resources to analysis specifications
#'
#' @param analysisSpecifications An object of type `AnalysisSpecifications` as created
#'                               by [createEmptyAnalysisSpecificiations()].
#' @param sharedResources   An object of type `SharedResources`.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
#' @export
addSharedResources <- function(analysisSpecifications, sharedResources) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(sharedResources, "SharedResources", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  analysisSpecifications$sharedResources[[length(analysisSpecifications$sharedResources) + 1]] <- sharedResources
  return(analysisSpecifications)
}

#' Add module specifications to analysis specifications
#'
#' @param analysisSpecifications An object of type `AnalysisSpecifications` as created
#'                               by [createEmptyAnalysisSpecificiations()].
#' @param moduleSpecifications   An object of type `ModuleSpecifications`.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
#' @export
addModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(moduleSpecifications, "ModuleSpecifications", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  analysisSpecifications$moduleSpecifications[[length(analysisSpecifications$moduleSpecifications) + 1]] <- moduleSpecifications
  return(analysisSpecifications)
}

#' Create CDM execution settings
#'
#' @param connectionDetailsReference A string that can be used to retrieve database connection details from a secure local
#'                                   store.
#' @param workDatabaseSchema         A database schema where intermediate data can be stored. The user (as identified in the
#'                                   connection details) will need to have write access to this database schema.
#' @param cdmDatabaseSchema          The database schema containing the data in CDM format. The user (as identified in the
#'                                   connection details) will need to have read access to this database schema.
#' @param cohortTableNames           An object identifying the various cohort table names that will be created in the
#'                                   `workDatabaseSchema`. This object can be created using the
#'                                   [CohortGenerator::getCohortTableNames()] function.
#' @param workFolder                 A folder in the local file system where intermediate results can be written.
#' @param resultsFolder              A folder in the local file system where the module output will be written.
#' @param minCellCount               The minimum number of subjects contributing to a count before it can be included
#'                                   in results.
#'
#' @return
#' An object of type `ExecutionSettings`.
#'
#' @export
createCdmExecutionSettings <- function(connectionDetailsReference,
                                       workDatabaseSchema,
                                       cdmDatabaseSchema,
                                       cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
                                       workFolder,
                                       resultsFolder,
                                       minCellCount = 5) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(connectionDetailsReference, len = 1, add = errorMessages)
  checkmate::assertCharacter(workDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(cohortTableNames, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsFolder, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  executionSettings <- list(connectionDetailsReference = connectionDetailsReference,
                            workDatabaseSchema = workDatabaseSchema,
                            cdmDatabaseSchema = cdmDatabaseSchema,
                            cohortTableNames = cohortTableNames,
                            workFolder = workFolder,
                            resultsFolder = resultsFolder,
                            minCellCount = minCellCount)
  class(executionSettings) <- "ExecutionSettings"
  return(executionSettings)
}

#' Create Results execution settings
#'
#' @param resultsConnectionDetailsReference A string that can be used to retrieve the results database connection
#'                                          details from a secure local store.
#' @param resultsDatabaseSchema      A schema where the results tables are stored
#' @param workFolder                 A folder in the local file system where intermediate results can be written.
#' @param resultsFolder              A folder in the local file system where the module output will be written.
#' @param minCellCount               The minimum number of subjects contributing to a count before it can be included
#'                                   in results.
#'
#' @return
#' An object of type `ExecutionSettings`.
#'
#' @export
createResultsExecutionSettings <- function(resultsConnectionDetailsReference,
                                           resultsDatabaseSchema,
                                           workFolder,
                                           resultsFolder,
                                           minCellCount = 5) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(resultsConnectionDetailsReference, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsFolder, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  executionSettings <- list(resultsConnectionDetailsReference = resultsConnectionDetailsReference,
                            resultsDatabaseSchema = resultsDatabaseSchema,
                            workFolder = workFolder,
                            resultsFolder = resultsFolder,
                            minCellCount = minCellCount)
  class(executionSettings) <- "ExecutionSettings"
  return(executionSettings)
}



# Note: assuming connectionDetails objects remain stable across the various module
# versions.

#' Store connection details in a secure location
#'
#' @param connectionDetails          An object of type `connectionDetails` as created by the
#'                                   [DatabaseConnector::createConnectionDetails()] function.
#' @param connectionDetailsReference  A string that can be used to retrieve the settings from
#'                                    the secure store.
#'
#' @seealso [retrieveConnectionDetails()]
#'
#' @return
#' Does not return anything. Is called for the side effect of having the connection details
#' stored.
#'
#' @export
storeConnectionDetails <- function(connectionDetails, connectionDetailsReference) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "connectionDetails", add = errorMessages)
  checkmate::assertCharacter(connectionDetailsReference, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # Evaluate functions used to secure details to allow serialization:
  for (i in 1:length(connectionDetails)) {
    if (is.function(connectionDetails[[i]])) {
      detail <- connectionDetails[[i]]()
      if (is.null(detail)) {
        connectionDetails[[i]] <- ""
      } else {
        connectionDetails[[i]] <- connectionDetails[[i]]()
      }
    }
  }
  connectionDetails <- ParallelLogger::convertSettingsToJson(connectionDetails)
  keyring::key_set_with_value(connectionDetailsReference, password = connectionDetails)
  invisible(NULL)
}

#' Retrieve connection details from the secure location
#'
#' @param connectionDetailsReference  A string that can be used to retrieve the settings from
#'                                    the secure store.
#'
#' @seealso [storeConnectionDetails()]
#'
#' @return
#' Returns an object of type `connectionDetails`.
#'
#' @export
retrieveConnectionDetails <- function(connectionDetailsReference) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(connectionDetailsReference, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  connectionDetails <- keyring::key_get(connectionDetailsReference)
  connectionDetails <- ParallelLogger::convertJsonToSettings(connectionDetails)
  connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, connectionDetails)
  return(connectionDetails)
}
