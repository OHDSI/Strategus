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

#' Create Result Data Models
#' @description
#' Use this at the study design stage to create data models for modules
#' This functions loads modules and executes any custom code to create schemas in a results database
#' If recreate is set to TRUE all existing data will be removed, otherwise
#'
#' @inheritParams execute
#'
#' @export
createResultDataModels <- function(analysisSpecifications,
                                   resultsExecutionSettings,
                                   resultsConnectionDetails) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(resultsExecutionSettings, "ResultsExecutionSettings", add = errorMessages)
  checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # The DatabaseMetaData is a special case...
  .createDatabaseMetadataTables(
    resultsConnectionDetails = resultsConnectionDetails,
    resultsDatabaseSchema = resultsExecutionSettings$resultsDatabaseSchema,
    resultsFolder = resultsExecutionSettings$resultsFolder
  )

  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    moduleObj <- get(moduleName)$new()
    moduleObj$createResultsSchema(
      resultsConnectionDetails = resultsConnectionDetails,
      resultsSchema = resultsExecutionSettings$resultsDatabaseSchema,
    )
  }
}

#' Upload results
#'
#' @description
#'
#' Upload the results for a given analysis
#'
#' @inheritParams createResultDataModels
#'
#' @export
uploadResults <- function(analysisSpecifications,
                          resultsExecutionSettings,
                          resultsConnectionDetails) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(resultsExecutionSettings, "ResultsExecutionSettings", add = errorMessages)
  checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # The DatabaseMetaData is a special case...
  .uploadDatabaseMetadata(
    resultsConnectionDetails = resultsConnectionDetails,
    resultsDatabaseSchema = resultsExecutionSettings$resultsDatabaseSchema,
    resultsFolder = resultsExecutionSettings$resultsFolder
  )

  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    moduleObj <- get(moduleName)$new()
    moduleObj$uploadResults(
      resultsConnectionDetails = resultsConnectionDetails,
      analysisSpecifications = analysisSpecifications,
      resultsExecutionSettings = resultsExecutionSettings
    )
  }
}


