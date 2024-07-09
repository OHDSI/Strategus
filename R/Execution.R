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

#' Execute analysis specifications.
#'
#' @template AnalysisSpecifications
#' @template executionSettings
#' @param executionScriptFolder   Optional: the path to use for storing the execution script.
#'                                when NULL, this function will use a temporary
#'                                file location to create the script to execute.
#' @param restart                 Restart run? Requires `executionScriptFolder` to be specified, and be
#'                                the same as the `executionScriptFolder` used in the run to restart.
#'
#' @template enforceModuleDependencies
#'
#' @return
#' Does not return anything. Is called for the side-effect of executing the specified
#' analyses.
#'
#' @export
execute <- function(analysisSpecifications,
                    executionSettings,
                    connectionDetails) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(executionSettings, "ExecutionSettings", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # Validate the execution settings
  if (is(executionSettings, "CdmExecutionSettings")) {
    # Assert that the temp emulation schema is set if required for the dbms
    # specified by the executionSettings
    DatabaseConnector::assertTempEmulationSchemaSet(
      dbms = connectionDetails$dbms,
      tempEmulationSchema = executionSettings$tempEmulationSchema
    )

    # Validate that the table names specified in the execution settings
    # do not violate the maximum table length. To do this we will render
    # a query using the execution settings so that SqlRender can provide
    # the appropriate warning. Only stop execution if we are running on
    # Oracle; otherwise it is unclear if the table name length will have
    # an impact
    cohortTableChecks <- lapply(
      X = executionSettings$cohortTableNames,
      FUN = function(tableName) {
        sql <- SqlRender::render(
          sql = "CREATE TABLE @table;",
          table = tableName
        )
        tryCatch(
          {
            SqlRender::translate(
              sql = sql,
              targetDialect = connectionDetails$dbms
            )
            return(TRUE)
          },
          warning = function(w) {
            warning(w)
            return(FALSE)
          }
        )
      }
    )

    # Since the warning is thrown for all dbms systems, only stop if
    # we are executing on Oracle
    if (tolower(connectionDetails$dbms) == "oracle" && !all(unlist(cohortTableChecks))) {
      stop("Your cohort table names are too long for Oracle. Please update your executionSettings to use shorter cohort table names and try again.")
    }
  }

  # Set up logging
  if (!dir.exists(dirname(executionSettings$logFileName))) {
    dir.create(dirname(executionSettings$logFileName), recursive = T)
  }
  ParallelLogger::addDefaultFileLogger(
    name = "STRATEGUS_LOGGER",
    fileName = executionSettings$logFileName
  )
  on.exit(ParallelLogger::unregisterLogger("STRATEGUS_LOGGER"))

  if (is(executionSettings, "CdmExecutionSettings")) {
    executionSettings$databaseId <- createDatabaseMetaData(
      executionSettings = executionSettings,
      connectionDetails = connectionDetails
    )
  }

  # Execute the cohort generator module first if it exists
  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    if (tolower(moduleName) == "cohortgeneratormodule") {
      cg <- CohortGeneratorModule$new()
      cg$execute(
        connectionDetails = connectionDetails,
        analysisSpecifications = analysisSpecifications,
        executionSettings = executionSettings
      )
      break;
    }
  }

  # Execute any other modules
  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    if (tolower(moduleName) != "cohortgeneratormodule") {
      moduleObj <- get(moduleName)$new()
      moduleObj$execute(
        connectionDetails = connectionDetails,
        analysisSpecifications = analysisSpecifications,
        executionSettings = executionSettings
      )
    }
  }
}
