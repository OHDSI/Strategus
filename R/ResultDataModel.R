# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' Create Result Data Model
#'
#' @description
#' Use this at the study design stage to create data models for modules
#' This functions loads modules and executes any custom code to create
#' the results data model in the specified schema in the results database.
#'
#' @template AnalysisSpecifications
#' @param resultsDataModelSettings The results data model settings as created using [@seealso [createResultsDataModelSettings()]]
#' @template resultsConnectionDetails
#'
#' @export
createResultDataModel <- function(analysisSpecifications,
                                  resultsDataModelSettings,
                                  resultsConnectionDetails) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(resultsDataModelSettings, "ResultsDataModelSettings", add = errorMessages)
  checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # Used to keep track of the execution status
  executionStatus <- list()

  # The DatabaseMetaData is a special case...
  .createDatabaseMetadataResultsDataModel(
    resultsConnectionDetails = resultsConnectionDetails
  )

  # Determine if the user has opted to subset to specific modules
  # in the analysis specification. If so, validate that the
  # modulesToExecute are present in the analysis specification
  # before attempting to subset the analyses to run.
  if (length(resultsDataModelSettings$modulesToExecute) > 0) {
    analysisSpecifications <- .subsetAnalysisSpecificationByModulesToExecute(
      analysisSpecifications = analysisSpecifications,
      modulesToExecute = resultsDataModelSettings$modulesToExecute
    )
  }


  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    moduleExecutionStatus <- .resultDataModelModuleExecution(
      moduleName = moduleName,
      functionName = "createResultsDataModel",
      resultsConnectionDetails = resultsConnectionDetails,
      resultsDataModelSettings = resultsDataModelSettings
    )
    executionStatus <- append(
      executionStatus,
      moduleExecutionStatus
    )
  }

  # Print a summary
  .printExecutionSummary(
    summaryTitle = "RESULT DATA MODEL CREATION SUMMARY",
    executionStatus = executionStatus
  )

  invisible(executionStatus)
}

#' Upload results
#'
#' @description
#'
#' Upload the results for a given analysis
#'
#' @template AnalysisSpecifications
#' @template resultsDataModelSettings
#' @template resultsConnectionDetails
#'
#' @export
uploadResults <- function(analysisSpecifications,
                          resultsDataModelSettings,
                          resultsConnectionDetails) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(resultsDataModelSettings, "ResultsDataModelSettings", add = errorMessages)
  checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(resultsDataModelSettings$resultsFolder, len = 1)
  checkmate::reportAssertions(collection = errorMessages)

  # Used to keep track of the execution status
  executionStatus <- list()

  # The DatabaseMetaData is a special case...
  .uploadDatabaseMetadata(
    resultsConnectionDetails = resultsConnectionDetails,
    resultsDataModelSettings = resultsDataModelSettings
  )

  # Determine if the user has opted to subset to specific modules
  # in the analysis specification. If so, validate that the
  # modulesToExecute are present in the analysis specification
  # before attempting to subset the analyses to run.
  if (length(resultsDataModelSettings$modulesToExecute) > 0) {
    analysisSpecifications <- .subsetAnalysisSpecificationByModulesToExecute(
      analysisSpecifications = analysisSpecifications,
      modulesToExecute = resultsDataModelSettings$modulesToExecute
    )
  }

  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    moduleExecutionStatus <- .resultDataModelModuleExecution(
      moduleName = moduleName,
      functionName = "uploadResults",
      resultsConnectionDetails = resultsConnectionDetails,
      resultsDataModelSettings = resultsDataModelSettings,
      analysisSpecifications = analysisSpecifications
    )
    executionStatus <- append(
      executionStatus,
      moduleExecutionStatus
    )
  }

  # Print a summary
  .printExecutionSummary(
    summaryTitle = "UPLOAD SUMMARY",
    executionStatus = executionStatus
  )

  invisible(executionStatus)
}

.resultDataModelModuleExecution <- function(moduleName, functionName, resultsConnectionDetails, resultsDataModelSettings, analysisSpecifications = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertChoice(x = functionName, choices = c("createResultsDataModel", "uploadResults"))
  checkmate::reportAssertions(collection = errorMessages)

  moduleObject <- get(moduleName)$new()
  moduleFn <- moduleObject[[functionName]]
  if (functionName == "createResultsDataModel") {
    executionResult <- .safeExecution(
      fn = moduleFn,
      resultsConnectionDetails = resultsConnectionDetails,
      resultsDatabaseSchema = resultsDataModelSettings$resultsDatabaseSchema
    )
  } else {
    executionResult <- .safeExecution(
      fn = moduleFn,
      resultsConnectionDetails = resultsConnectionDetails,
      analysisSpecifications = analysisSpecifications,
      resultsDataModelSettings = resultsDataModelSettings
    )
  }
  if (executionResult$status == "FAILED") {
    .printErrorMessage(executionResult$error$message)
  }
  return(
    .createModuleExecutionStatus(
      moduleName = moduleName,
      status = executionResult$status,
      errorMessage = executionResult$error$message,
      executionTime = paste0(signif(executionResult$timeToExecute, 3), " ", attr(executionResult$timeToExecute, "units"))
    )
  )
}
