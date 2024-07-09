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

#' @export
addCharacterizationModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "CharacterizationModule",
      analysisSpecifications = analysisSpecifications,
      moduleSpecifications = moduleSpecifications
    )
  )
}

#' @export
addCohortDiagnosticsModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "CohortDiagnosticsModule",
      analysisSpecifications = analysisSpecifications,
      moduleSpecifications = moduleSpecifications
    )
  )
}

#' @export
addCohortGeneratorModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "CohortGeneratorModule",
      analysisSpecifications = analysisSpecifications,
      moduleSpecifications = moduleSpecifications
    )
  )
}

#' @export
addCohortIncidenceModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "CohortIncidenceModule",
      analysisSpecifications = analysisSpecifications,
      moduleSpecifications = moduleSpecifications
    )
  )
}

#' @export
addCohortMethodeModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "CohortMethodModule",
      analysisSpecifications = analysisSpecifications,
      moduleSpecifications = moduleSpecifications
    )
  )
}

#' @export
addEvidenceSynthesisModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "EvidenceSynthesisModule",
      analysisSpecifications = analysisSpecifications,
      moduleSpecifications = moduleSpecifications
    )
  )
}

#' @export
addPatientLevelPredictionModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "PatientLevelPredictionModule",
      analysisSpecifications = analysisSpecifications,
      moduleSpecifications = moduleSpecifications
    )
  )
}

#' @export
addSelfControlledCaseSeriesModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "SelfControlledCaseSeriesModule",
      analysisSpecifications = analysisSpecifications,
      moduleSpecifications = moduleSpecifications
    )
  )
}

addAndValidateModuleSpecifications <- function(moduleName, analysisSpecifications, moduleSpecifications) {
  moduleObj <- get(moduleName)$new()
  moduleObj$validateModuleSpecifications(moduleSpecifications)
  analysisSpecifications <- addModuleSpecifications(
    analysisSpecifications = analysisSpecifications,
    moduleSpecifications = moduleSpecifications
  )
  return(analysisSpecifications)
}

#' Create CDM execution settings
#'
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
#'
#' @return
#' An object of type `ExecutionSettings`.
#'
#' @export
createCdmExecutionSettings <- function(workDatabaseSchema,
                                       cdmDatabaseSchema,
                                       cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
                                       tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                       workFolder,
                                       resultsFolder,
                                       logFileName = file.path(resultsFolder, "strategus-log.txt"),
                                       minCellCount = 5,
                                       integerAsNumeric = getOption("databaseConnectorIntegerAsNumeric", default = TRUE),
                                       integer64AsNumeric = getOption("databaseConnectorInteger64AsNumeric", default = TRUE)) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(workDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(cohortTableNames, add = errorMessages)
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

  executionSettings <- list()
  for (name in names(formals(createCdmExecutionSettings))) {
    executionSettings[[name]] <- get(name)
  }
  class(executionSettings) <- c("CdmExecutionSettings", "ExecutionSettings")
  return(executionSettings)
}

#' Create Results execution settings
#'
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
createResultsExecutionSettings <- function(resultsDatabaseSchema,
                                           workFolder,
                                           resultsFolder,
                                           logFileName = file.path(resultsFolder, "strategus-log.txt"),
                                           minCellCount = 5,
                                           integerAsNumeric = getOption("databaseConnectorIntegerAsNumeric", default = TRUE),
                                           integer64AsNumeric = getOption("databaseConnectorInteger64AsNumeric", default = TRUE)) {
  errorMessages <- checkmate::makeAssertCollection()
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

  executionSettings <- list()
  for (name in names(formals(createResultsExecutionSettings))) {
    executionSettings[[name]] <- get(name)
  }
  class(executionSettings) <- c("ResultsExecutionSettings", "ExecutionSettings")
  return(executionSettings)
}

#' Create Results Data Model Settings
#'
#' @param resultsDatabaseSchema      A schema where the results tables are stored
#' @param resultsFolder              A folder in the local file system where the execution results are held.
#'                                   TODO: Why? Creating the results data model needs this parameter
#'                                   for the DatabaseMetaData which feels inconsistent.
#' @param logFileName                Logging information from the results data model creation
#'
#' @return
#' An object of type `ResultsDataModelSettings`
#'
#' @export
createResultsDataModelSettings <- function(resultsDatabaseSchema,
                                           resultsFolder,
                                           logFileName = file.path(resultsFolder, "strategus-results-data-model-log.txt")) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(resultsDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(logFileName, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # Normalize paths to convert relative paths to absolute paths
  resultsFolder <- normalizePath(resultsFolder, mustWork = F)
  logFileName <- normalizePath(logFileName, mustWork = F)

  executionSettings <- list()
  for (name in names(formals(createResultsDataModelSettings))) {
    executionSettings[[name]] <- get(name)
  }
  class(executionSettings) <- c("ResultsDataModelSettings")
  return(executionSettings)
}
