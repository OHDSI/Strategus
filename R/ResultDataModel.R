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

  # The DatabaseMetaData is a special case...
  .createDatabaseMetadataResultsDataModel(
    resultsConnectionDetails = resultsConnectionDetails,
    resultsDataModelSettings = resultsDataModelSettings
  )

  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    moduleObj <- get(moduleName)$new()
    moduleObj$createResultsDataModel(
      resultsConnectionDetails = resultsConnectionDetails,
      resultsDatabaseSchema = resultsDataModelSettings$resultsDatabaseSchema
    )
  }
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
  checkmate::reportAssertions(collection = errorMessages)

  # The DatabaseMetaData is a special case...
  .uploadDatabaseMetadata(
    resultsConnectionDetails = resultsConnectionDetails,
    resultsDataModelSettings = resultsDataModelSettings
  )

  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    moduleObj <- get(moduleName)$new()
    moduleObj$uploadResults(
      resultsConnectionDetails = resultsConnectionDetails,
      analysisSpecifications = analysisSpecifications,
      resultsDataModelSettings = resultsDataModelSettings
    )
  }
}
