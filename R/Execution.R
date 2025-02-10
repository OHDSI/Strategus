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

  # Set up logging
  if (!dir.exists(dirname(executionSettings$logFileName))) {
    dir.create(dirname(executionSettings$logFileName), recursive = T)
  }
  ParallelLogger::addDefaultFileLogger(
    name = "STRATEGUS_LOGGER",
    fileName = executionSettings$logFileName
  )
  on.exit(ParallelLogger::unregisterLogger("STRATEGUS_LOGGER"))

  # Used to keep track of the execution status
  executionStatus <- list()

  # Validate the execution settings
  if (is(executionSettings, "CdmExecutionSettings")) {
    message("Collecting OMOP CDM Metadata")

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

    # Obtain the database metadata for this CDM
    executionResult <- .safeExecution(
      fn = getCdmDatabaseMetaData,
      cdmExecutionSettings = executionSettings,
      connectionDetails = connectionDetails
    )
    databaseMetaDataSuccessful <- ifelse(executionResult$status == "SUCCESS", TRUE, FALSE)
    if (isTRUE(databaseMetaDataSuccessful)) {
      cdmDatabaseMetaData <- executionResult$result
      executionSettings$cdmDatabaseMetaData <- cdmDatabaseMetaData
      .writeDatabaseMetaData(cdmDatabaseMetaData, executionSettings)
    } else {
      errorMsg <- paste0("COULD NOT OBTAIN OMOP CDM Metadata! ERROR: ", executionResult$error)
      stop(errorMsg)
    }
  }


  # Determine if the user has opted to subset to specific modules
  # in the analysis specification. If so, validate that the
  # modulesToExecute are present in the analysis specification
  # before attempting to subset the analyses to run.
  if (length(executionSettings$modulesToExecute) > 0) {
    analysisSpecifications <- .subsetAnalysisSpecificationByModulesToExecute(
      analysisSpecifications = analysisSpecifications,
      modulesToExecute = executionSettings$modulesToExecute
    )
  }

  # Execute the cohort generator module first if it exists
  # If cohort generation fails for any reason, update the
  # cohortGenerationSuccessful flag to FALSE so that subsequent
  # modules are skipped
  cohortGenerationSuccessful <- TRUE
  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    if (tolower(moduleName) == "cohortgeneratormodule") {
      moduleExecutionStatus <- .executeModule(
        moduleName = moduleName,
        connectionDetails = connectionDetails,
        analysisSpecifications = analysisSpecifications,
        executionSettings = executionSettings
      )
      # The absence of an error in moduleExecutionStatus$error
      # represents a success
      cohortGenerationSuccessful <- ifelse(moduleExecutionStatus[[1]]$status == "SUCCESS", TRUE, FALSE)
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
        executionSettings = executionSettings,
        skipExecution = !cohortGenerationSuccessful
      )
      executionStatus <- append(
        executionStatus,
        moduleExecutionStatus
      )
    } else {
      executionStatus <- append(
        executionStatus,
        moduleExecutionStatus
      )
    }
  }

  # Print a summary
  .printExecutionSummary(
    summaryTitle = "EXECUTION SUMMARY",
    executionStatus = executionStatus
  )

  invisible(executionStatus)
}

.printExecutionSummary <- function(summaryTitle, executionStatus) {
  cli::cli_h1(summaryTitle)
  for (i in 1:length(executionStatus)) {
    moduleStatus <- executionStatus[[i]]
    errorMessage <- ifelse(moduleStatus$status == "SUCCESS", "", moduleStatus$errorMessage)
    moduleStatusMessage <- sprintf("%s %s (Execution Time: %s)", moduleStatus$moduleName, errorMessage, moduleStatus$executionTime)
    if (moduleStatus$status == "FAILED") {
      cli::cli_alert_danger(moduleStatusMessage)
    } else if (moduleStatus$status == "SKIPPED") {
      cli::cli_alert_warning(moduleStatusMessage)
    } else {
      cli::cli_alert_success(moduleStatusMessage)
    }
  }
}

.safeExecution <- function(fn, ...) {
  safeExec <- purrr::safely(fn)
  startTime <- Sys.time()
  functionResult <- safeExec(...)
  timeToExecute <- Sys.time() - startTime
  # Emit any errors
  status <- ifelse(is.null(functionResult$error), "SUCCESS", "FAILED")
  return(list(
    result = functionResult$result,
    error = functionResult$error,
    timeToExecute = timeToExecute,
    status = status
  ))
}

.executeModule <- function(moduleName, connectionDetails, analysisSpecifications, executionSettings, skipExecution = FALSE) {
  if (isFALSE(skipExecution)) {
    moduleObject <- get(moduleName)$new()
    executionResult <- .safeExecution(
      fn = moduleObject$execute,
      connectionDetails = connectionDetails,
      analysisSpecifications = analysisSpecifications,
      executionSettings = executionSettings
    )
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
  } else {
    return(
      .createModuleExecutionStatus(
        moduleName = moduleName,
        status = "SKIPPED",
        errorMessage = "Cohort generation failed",
        executionTime = "SKIPPED"
      )
    )
  }
}

.createModuleExecutionStatus <- function(moduleName, status, errorMessage, executionTime) {
  return(
    list(
      list(
        moduleName = moduleName,
        status = status,
        errorMessage = errorMessage,
        executionTime = executionTime
      )
    )
  )
}

.printErrorMessage <- function(message) {
  error <- cli::combine_ansi_styles("red")
  cat(error(paste0("ERROR: ", message, "\n")))
}

.subsetAnalysisSpecificationByModulesToExecute <- function(analysisSpecifications, modulesToExecute) {
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

  modulesToExecuteString <- paste(modulesToExecute, collapse = ", ")
  modulesInAnalysisSpecificationString <- paste(modulesInAnalysisSpecification, collapse = ", ")

  # Stop if we cannot find all of the requested modules
  # to execute in the overall analysis specification
  if (!all(tolower(modulesToExecute) %in% tolower(modulesInAnalysisSpecification))) {
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
        if (tolower(x$module) %in% tolower(modulesToExecute)) {
          return(x$idx)
        }
      }
    )
  )
  analysisSpecifications$moduleSpecifications <- analysisSpecifications$moduleSpecifications[moduleSubset]
  return(analysisSpecifications)
}
