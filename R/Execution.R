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

#' Execute analysis specifications.
#'
#' @template AnalysisSpecifications
#' @template executionSettings
#' @template connectionDetails
#'
#' @return
#' Returns a list of lists that contains
#' - moduleName: The name of the module executed
#' - result: The result of the execution. See purrr::safely for details on
#' this result.
#' - executionTime: The time for the module to execute
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


  # Determine if the user has opted to subset to specific modules
  # in the analysis specification. If so, validate that the
  # modulesToExecute are present in the analysis specification
  # before attempting to subset the analyses to run.
  if (length(executionSettings$modulesToExecute) > 0) {
    # Get the modules in the analysis specification with their
    # index in the array
    modulesWithIndex <- lapply(
      X = seq_along(analysisSpecifications$moduleSpecifications),
      FUN = function(i) {
        list(
          idx = i,
          module = analysisSpecifications$moduleSpecifications[[i]]$module
        )
      }
    )
    modulesInAnalysisSpecification <- sapply(
      X = modulesWithIndex,
      FUN = function(x) {
        x$module
      }
    )

    modulesToExecuteString <- paste(executionSettings$modulesToExecute, collapse = ", ")
    modulesInAnalysisSpecificationString <- paste(modulesInAnalysisSpecification, collapse = ", ")

    # Stop if we cannot find all of the requested modules
    # to execute in the overall analysis specification
    if (!all(tolower(executionSettings$modulesToExecute) %in% tolower(modulesInAnalysisSpecification))) {
      errorMsg <- paste0(
        "The executionSettings specified to run only the modules: ",
        modulesToExecuteString,
        ".\n However the analysis specification includes the following modules: ",
        modulesInAnalysisSpecificationString
      )
      stop(errorMsg)
    }

    # Subset the analysis specifications to those modules
    # specified by the user
    cli::cli_alert_info(paste0("Runnning a subset of modules: ", modulesToExecuteString))
    moduleSubset <- unlist(
      lapply(
        X = modulesWithIndex,
        FUN = function(x) {
          if (tolower(x$module) %in% tolower(executionSettings$modulesToExecute)) {
            return(x$idx)
          }
        }
      )
    )
    analysisSpecifications$moduleSpecifications <- analysisSpecifications$moduleSpecifications[moduleSubset]
  }

  if (is(executionSettings, "CdmExecutionSettings")) {
    cdmDatabaseMetaData <- getCdmDatabaseMetaData(
      cdmExecutionSettings = executionSettings,
      connectionDetails = connectionDetails
    )
    executionSettings$cdmDatabaseMetaData <- cdmDatabaseMetaData
    .writeDatabaseMetaData(cdmDatabaseMetaData, executionSettings)
  }

  executionStatus <- list()

  # Execute the cohort generator module first if it exists
  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    if (tolower(moduleName) == "cohortgeneratormodule") {
      moduleExecutionStatus <- .executeModule(
        moduleName = moduleName,
        connectionDetails = connectionDetails,
        analysisSpecifications = analysisSpecifications,
        executionSettings = executionSettings
      )
      executionStatus <- append(
        executionStatus,
        moduleExecutionStatus
      )
      break
    }
  }

  # Execute any other modules
  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    if (tolower(moduleName) != "cohortgeneratormodule") {
      moduleExecutionStatus <- .executeModule(
        moduleName = moduleName,
        connectionDetails = connectionDetails,
        analysisSpecifications = analysisSpecifications,
        executionSettings = executionSettings
      )
      executionStatus <- append(
        executionStatus,
        moduleExecutionStatus
      )
    }
  }

  # Print a summary
  cli::cli_h1("EXECUTION SUMMARY")
  for (i in 1:length(executionStatus)) {
    status <- executionStatus[[i]]
    errorMessage <- ifelse(!is.null(status$result$error), status$result$error, "")
    statusMessage <- sprintf("%s %s (Execution Time: %s)", status$moduleName, errorMessage, status$executionTime)
    if (!is.null(status$result$error)) {
      cli::cli_alert_danger(statusMessage)
    } else {
      cli::cli_alert_success(statusMessage)
    }
  }

  invisible(executionStatus)
}

.executeModule <- function(moduleName, connectionDetails, analysisSpecifications, executionSettings) {
  moduleObject <- get(moduleName)$new()
  safeExec <- purrr::safely(moduleObject$execute)
  startTime <- Sys.time()
  executionResult <- safeExec(
    connectionDetails = connectionDetails,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )
  timeToExecute <- Sys.time() - startTime
  # Emit any errors
  if (!is.null(executionResult$error)) {
    .printErrorMessage(executionResult$error$message)
  }
  return(
    list(
      list(
        moduleName = moduleName,
        result = executionResult,
        executionTime = paste0(signif(timeToExecute, 3), " ", attr(timeToExecute, "units"))
      )
    )
  )
}

.printErrorMessage <- function(message) {
  error <- cli::combine_ansi_styles("red")
  cat(error(paste0("ERROR: ", message, "\n")))
}
