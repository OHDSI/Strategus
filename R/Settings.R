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

#' Create an empty analysis specifications object.
#'
#' @return
#' An object of type `AnalysisSpecifications`.
#'
#' @export
createEmptyAnalysisSpecificiations <- function() {
  analysisSpecifications <- list(
    sharedResources = list(),
    moduleSpecifications = list()
  )
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
#' @param tempEmulationSchema        Some database platforms like Oracle and Impala do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created.
#' @param workFolder                 A folder in the local file system where intermediate results can be written.
#' @param resultsFolder              A folder in the local file system where the module output will be written.
#' @param logFileName                Logging information from Strategus and all modules will be located in this file. Individual modules will continue to have their own module-specific logs. By default this will be written to the root of the `resultsFolder`
#' @param minCellCount               The minimum number of subjects contributing to a count before it can be included
#'                                   in results.
#' @param integerAsNumeric           Logical: should 32-bit integers be converted to numeric (double) values? If FALSE 32-bit integers will be represented using R's native `Integer` class. Default is TRUE
#' @param integer64AsNumeric         Logical: should 64-bit integers be converted to numeric (double) values? If FALSE 64-bit integers will be represented using `bit64::integer64`.  Default is TRUE
#' @param resultsConnectionDetailsReference A string that can be used to retrieve the results database connection
#'                                          details from a secure local store.
#' @param resultsDatabaseSchema      A schema where the results tables are stored
#'
#' @return
#' An object of type `ExecutionSettings`.
#'
#' @export
createCdmExecutionSettings <- function(connectionDetailsReference,
                                       workDatabaseSchema,
                                       cdmDatabaseSchema,
                                       cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
                                       tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                       workFolder,
                                       resultsFolder,
                                       logFileName = file.path(resultsFolder, "strategus-log.txt"),
                                       minCellCount = 5,
                                       integerAsNumeric = getOption("databaseConnectorIntegerAsNumeric", default = TRUE),
                                       integer64AsNumeric = getOption("databaseConnectorInteger64AsNumeric", default = TRUE),
                                       resultsConnectionDetailsReference = NULL,
                                       resultsDatabaseSchema = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(connectionDetailsReference, len = 1, add = errorMessages)
  checkmate::assertCharacter(workDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(cohortTableNames, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(logFileName, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, add = errorMessages)
  checkmate::assertLogical(integerAsNumeric, max.len = 1, add = errorMessages)
  checkmate::assertLogical(integer64AsNumeric, max.len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsConnectionDetailsReference, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(resultsDatabaseSchema, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # Normalize paths to convert relative paths to absolute paths
  workFolder <- normalizePath(workFolder, mustWork = F)
  resultsFolder <- normalizePath(resultsFolder, mustWork = F)
  logFileName <- normalizePath(logFileName, mustWork = F)

  executionSettings <- list(
    connectionDetailsReference = connectionDetailsReference,
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = cohortTableNames,
    tempEmulationSchema = tempEmulationSchema,
    workFolder = workFolder,
    resultsFolder = resultsFolder,
    logFileName = logFileName,
    minCellCount = minCellCount,
    integerAsNumeric = integerAsNumeric,
    integer64AsNumeric = integer64AsNumeric,
    resultsConnectionDetailsReference = resultsConnectionDetailsReference,
    resultsDatabaseSchema = resultsDatabaseSchema
  )
  class(executionSettings) <- c("CdmExecutionSettings", "ExecutionSettings")
  return(executionSettings)
}

#' Create Results execution settings
#'
#' @param resultsConnectionDetailsReference A string that can be used to retrieve the results database connection
#'                                          details from a secure local store.
#' @param resultsDatabaseSchema      A schema where the results tables are stored
#' @param workFolder                 A folder in the local file system where intermediate results can be written.
#' @param resultsFolder              A folder in the local file system where the module output will be written.
#' @param logFileName                Logging information from Strategus and all modules will be located in this file. Individual modules will continue to have their own module-specific logs. By default this will be written to the root of the `resultsFolder`
#' @param minCellCount               The minimum number of subjects contributing to a count before it can be included
#'                                   in results.
#' @param integerAsNumeric           Logical: should 32-bit integers be converted to numeric (double) values? If FALSE 32-bit integers will be represented using R's native `Integer` class. Default is TRUE
#' @param integer64AsNumeric         Logical: should 64-bit integers be converted to numeric (double) values? If FALSE 64-bit integers will be represented using `bit64::integer64`.  Default is TRUE
#'
#' @return
#' An object of type `ExecutionSettings`.
#'
#' @export
createResultsExecutionSettings <- function(resultsConnectionDetailsReference,
                                           resultsDatabaseSchema,
                                           workFolder,
                                           resultsFolder,
                                           logFileName = file.path(resultsFolder, "strategus-log.txt"),
                                           minCellCount = 5,
                                           integerAsNumeric = getOption("databaseConnectorIntegerAsNumeric", default = TRUE),
                                           integer64AsNumeric = getOption("databaseConnectorInteger64AsNumeric", default = TRUE)) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(resultsConnectionDetailsReference, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(logFileName, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, add = errorMessages)
  checkmate::assertLogical(integerAsNumeric, max.len = 1, add = errorMessages)
  checkmate::assertLogical(integer64AsNumeric, max.len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # Normalize paths to convert relative paths to absolute paths
  workFolder <- normalizePath(workFolder, mustWork = F)
  resultsFolder <- normalizePath(resultsFolder, mustWork = F)
  logFileName <- normalizePath(logFileName, mustWork = F)

  executionSettings <- list(
    resultsConnectionDetailsReference = resultsConnectionDetailsReference,
    resultsDatabaseSchema = resultsDatabaseSchema,
    workFolder = workFolder,
    resultsFolder = resultsFolder,
    logFileName = logFileName,
    minCellCount = minCellCount,
    integerAsNumeric = integerAsNumeric,
    integer64AsNumeric = integer64AsNumeric
  )
  class(executionSettings) <- c("ResultsExecutionSettings", "ExecutionSettings")
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
#' @template keyringName
#'
#' @seealso [retrieveConnectionDetails()]
#'
#' @return
#' Does not return anything. Is called for the side effect of having the connection details
#' stored.
#'
#' @export
storeConnectionDetails <- function(connectionDetails, connectionDetailsReference, keyringName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  # Get the keyring list and verify that the keyring specified exists.
  # In the case of the default NULL keyring, this will be represented as an empty
  # string in the keyring list
  keyringList <- keyring::keyring_list()
  if (is(connectionDetails, "connectionDetails")) {
    checkmate::assertClass(connectionDetails, "connectionDetails", add = errorMessages)
  } else {
    checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  }
  checkmate::assertCharacter(connectionDetailsReference, len = 1, add = errorMessages)
  checkmate::assertChoice(x = keyringName, choices = keyringList$keyring, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # Evaluate functions used to secure details to allow serialization:
  for (i in 1:length(connectionDetails)) {
    if (is.function(connectionDetails[[i]])) {
      detail <- connectionDetails[[i]]()
      if (is.null(detail)) {
        connectionDetails[[i]] <- .nullList() # Fixes Issue #74
      } else {
        connectionDetails[[i]] <- connectionDetails[[i]]()
      }
    }
  }
  connectionDetails <- ParallelLogger::convertSettingsToJson(connectionDetails)
  # If the keyring is locked, unlock it, set the value and then re-lock it
  keyringLocked <- unlockKeyring(keyringName = keyringName)
  keyring::key_set_with_value(connectionDetailsReference, password = connectionDetails, keyring = keyringName)
  if (keyringLocked) {
    keyring::keyring_lock(keyring = keyringName)
  }
  invisible(NULL)
}

#' Retrieve connection details from the secure location
#'
#' @param connectionDetailsReference  A string that can be used to retrieve the settings from
#'                                    the secure store.
#'
#' @template keyringName
#'
#' @seealso [storeConnectionDetails()]
#'
#' @return
#' Returns an object of type `connectionDetails`.
#'
#' @export
retrieveConnectionDetails <- function(connectionDetailsReference, keyringName = NULL) {
  keyringList <- keyring::keyring_list()
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(connectionDetailsReference, len = 1, add = errorMessages)
  checkmate::assertLogical(x = (is.null(keyringName) || keyringName %in% keyringList$keyring), add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!connectionDetailsReference %in% keyring::key_list(keyring = keyringName)$service) {
    stop("Connection details with connectionDetailsReference = \"", connectionDetailsReference, "\" were not found in your keyring. Please check that you have used the Strategus storeConnectionDetails function to save your connection details with this connectionDetailsReference name.")
  }

  # If the keyring is locked, unlock it, set the value and then re-lock it
  keyringLocked <- unlockKeyring(keyringName = keyringName)

  connectionDetails <- keyring::key_get(connectionDetailsReference, keyring = keyringName)
  connectionDetails <- ParallelLogger::convertJsonToSettings(connectionDetails)

  # Ensure that NA values are converted to NULL prior to calling
  # DatabaseConnector. To do this, we'll construct a new connectionDetails
  # list from keyring where the connectionDetails are NOT NA. This will
  # allow for calling DatabaseConnector::createConnectionDetails with
  # NULL values where NAs are present in the serialized version of the
  # connectionDetails from keyring.
  connectionDetailsConstructedFromKeyring <- list()
  for (i in 1:length(connectionDetails)) {
    if (isFALSE(is.na(connectionDetails[[i]]))) {
      connectionDetailsConstructedFromKeyring[names(connectionDetails)[i]] <- connectionDetails[[i]]
    }
  }

  connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, connectionDetailsConstructedFromKeyring)

  if (keyringLocked) {
    keyring::keyring_lock(keyring = keyringName)
  }

  return(connectionDetails)
}

#' Provides a list of HADES modules to run through Strategus
#'
#' @description
#' This function provides a list of modules and their locations
#' that may be used with Strategus.
#'
#' @return
#' A data.frame() of modules that work with Strategus. This will contain:
#' module = The name of the module
#' version = The version of the module
#' remote_repo = The remote location of the module (i.e. github.com)
#' remote_username = The organization of the module (i.e. OHDSI)
#' module_type = 'cdm' or 'results'. 'cdm' refers to modules that are designed to work against
#' patient level data in the OMOP CDM format. 'results' refers to modules that are designed
#' to work against a results database containing output from a 'cdm' module.
#'
#' @export
getModuleList <- function() {
  moduleList <- CohortGenerator::readCsv(file = system.file("csv/modules.csv",
    package = "Strategus",
    mustWork = TRUE
  ))
  return(moduleList)
}

#' Helper function to unlock a keyring
#'
#' @description
#' This helper function is used to unlock a keyring by using the password
#' stored in Sys.getenv("STRATEGUS_KEYRING_PASSWORD"). It will alert
#' the user if the environment variable with the password is not set.
#'
#' @template keyringName
#'
#' @return
#' Returns TRUE if the keyring was unlocked using the password otherwise
#' it returns FALSE
#'
#' @export
unlockKeyring <- function(keyringName) {
  # If the keyring is locked, unlock it, set the value and then re-lock it
  keyringLocked <- keyring::keyring_is_locked(keyring = keyringName)
  if (keyringLocked) {
    x <- Sys.getenv("STRATEGUS_KEYRING_PASSWORD")
    if (length(x) == 0 || x == "") {
      stop(paste0("STRATEGUS_KEYRING_PASSWORD NOT FOUND. STRATEGUS_KEYRING_PASSWORD must be set using Sys.setenv(STRATEGUS_KEYRING_PASSWORD = \"<your password>\") to unlock the keyring: ", keyringName))
    }
    keyring::keyring_unlock(keyring = keyringName, password = Sys.getenv("STRATEGUS_KEYRING_PASSWORD"))
  }
  return(keyringLocked)
}

#' @keywords internal
.checkModuleFolderSetting <- function(x) {
  if (length(x) == 0 || x == "") {
    return(paste0("INSTANTIATED_MODULES_FOLDER environment variable has not been set. INSTANTIATED_MODULES_FOLDER must be set using Sys.setenv(INSTANTIATED_MODULES_FOLDER = \"/somepath\")"))
  } else {
    return(TRUE)
  }
}

#' Used when serializing connection details to retain NULL values
#'
#' @keywords internal
.nullList <- function() {
  invisible(list(NULL))
}
