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

#' Add shared resources (i.e. cohorts) to analysis specifications
#'
#' @template analysisSpecifications
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

#' Add generic module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @template moduleSpecifications
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

#' Add Characterization module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications   Created by the \href{../../docs/reference/CharacterizationModule.html#method-CharacterizationModule-createModuleSpecifications}{\code{CharacterizationModule$createModuleSpecifications()}} function.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
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

#' Add Cohort Diagnostics module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications  Created by the \href{../../docs/reference/CohortDiagnosticsModule.html#method-CohortDiagnosticsModule-createModuleSpecifications}{\code{CohortDiagnosticsModule$createModuleSpecifications()}} function.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
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

#' Add Cohort Generator module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications  Created by the \href{../../docs/reference/CohortGeneratorModule.html#method-CohortGeneratorModule-createModuleSpecifications}{\code{CohortGeneratorModule$createModuleSpecifications()}} function.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
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

#' Add Cohort Incidence module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications  Created by the \href{../../docs/reference/CohortIncidenceModule.html#method-CohortIncidenceModule-createModuleSpecifications}{\code{CohortIncidenceModule$createModuleSpecifications()}} function.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
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

#' Add Cohort Method module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications  Created by the \href{../../docs/reference/CohortMethodModule.html#method-CohortMethodModule-createModuleSpecifications}{\code{CohortMethodModule$createModuleSpecifications()}} function.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
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

#' Add Evidence Synthesis module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications  Created by the \href{../../docs/reference/EvidenceSynthesisModule.html#method-EvidenceSynthesisModule-createModuleSpecifications}{\code{EvidenceSynthesisModule$createModuleSpecifications()}} function.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
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

#' Add Patient Level Prediction module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications  Created by the \href{../../docs/reference/PatientLevelPredictionModule.html#method-PatientLevelPredictionModule-createModuleSpecifications}{\code{PatientLevelPredictionModule$createModuleSpecifications()}} function.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
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

#' Add Self Controlled Case Series Module module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications  Created by the \href{../../docs/reference/SelfControlledCaseSeriesModule.html#method-SelfControlledCaseSeriesModule-createModuleSpecifications}{\code{SelfControlledCaseSeriesModule$createModuleSpecifications()}} function.
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added.
#'
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

#' Add Treatment Patterns Module specifications to analysis specifications
#'
#' @template analysisSpecifications
#' @param moduleSpecifications Created by the "tbd"
#'
#' @return
#' Returns the `analysisSpecifications` object with the module specifications added
#'
#' @export
addTreatmentPatternsModuleSpecifications <- function(analysisSpecifications, moduleSpecifications) {
  return(
    addAndValidateModuleSpecifications(
      moduleName = "TreatmentPatternsModule",
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
#' @template resultsFolder
#' @param logFileName                Logging information from Strategus and all modules will be located in this file. Individual modules will continue to have their own module-specific logs. By default this will be written to the root of the `resultsFolder`
#' @param minCellCount               The minimum number of subjects contributing to a count before it can be included
#'                                   in results.
#' @param incremental                This value will be passed to each module that supports execution in an incremental manner. Modules
#'                                   and their underlying packages may use the `workFolder` contents to determine their state of execution
#'                                   and attempt to pick up where they left off when this value is set to TRUE.
#' @param maxCores                   The maximum number of processing cores to use for execution. The default is to
#'                                   use all available cores on the machine.
#' @param modulesToExecute           (Optional) A vector with the list of modules to execute. When an empty vector/NULL is supplied (default),
#'                                   all modules in the analysis specification are executed.
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
                                       incremental = TRUE,
                                       maxCores = parallel::detectCores(),
                                       modulesToExecute = c()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(workDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertList(cohortTableNames, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(logFileName, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, add = errorMessages)
  checkmate::assertLogical(incremental, add = errorMessages)
  checkmate::assertInt(maxCores, add = errorMessages)
  checkmate::assertVector(modulesToExecute, null.ok = TRUE, add = errorMessages)
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
#' @template resultsDatabaseSchema
#' @param workFolder                 A folder in the local file system where intermediate results can be written.
#' @template resultsFolder
#' @param logFileName                Logging information from Strategus and all modules will be located in this file. Individual modules will continue to have their own module-specific logs. By default this will be written to the root of the `resultsFolder`
#' @param minCellCount               The minimum number of subjects contributing to a count before it can be included
#'                                   in results.
#' @param maxCores                   The maximum number of processing cores to use for execution. The default is to
#'                                   use all available cores on the machine.
#' @param modulesToExecute           (Optional) A vector with the list of modules to execute. When an empty vector/NULL is supplied (default),
#'                                   all modules in the analysis specification are executed.
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
                                           maxCores = parallel::detectCores(),
                                           modulesToExecute = c()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(resultsDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(workFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(resultsFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(logFileName, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, add = errorMessages)
  checkmate::assertInt(maxCores, add = errorMessages)
  checkmate::assertVector(modulesToExecute, null.ok = TRUE, add = errorMessages)
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
#' @description
#' The results data model settings are used to create the results data
#' model and to upload results.
#'
#' @template resultsDatabaseSchema
#' @template resultsFolder
#' @param logFileName     Log location for data model operations
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
