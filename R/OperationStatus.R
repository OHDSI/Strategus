# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Strategus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0

.OPERATION_STATUS_SCHEMA_VERSION <- 1L
.OPERATION_CHECKSUM_ALGORITHM <- "md5"
.SUPPORTED_OPERATIONS <- c("EXECUTION", "UPLOAD")

.getHadesModuleRegistry <- function() {
  CohortGenerator::readCsv(
    file = system.file(
      file.path("csv", "hadesModuleList.csv"),
      package = "Strategus",
      mustWork = TRUE
    ),
    warnOnCaseMismatch = FALSE
  )
}

.getModulePackageName <- function(moduleName) {
  moduleRegistry <- .getHadesModuleRegistry()
  index <- match(tolower(moduleName), tolower(moduleRegistry$module))
  if (is.na(index)) {
    stop("No underlying package is registered for module: ", moduleName, call. = FALSE)
  }
  moduleRegistry$package[[index]]
}

.getInstalledPackageVersion <- function(packageName) {
  if (!requireNamespace(packageName, quietly = TRUE)) {
    stop("Package '", packageName, "' must be installed to compute task status.", call. = FALSE)
  }
  as.character(utils::packageVersion(packageName))
}

.getStrategusVersion <- function() {
  as.character(utils::packageVersion("Strategus"))
}

.stabilizeOperationIdentity <- function(value) {
  if (is.data.frame(value)) {
    value[] <- lapply(value, .stabilizeOperationIdentity)
    if (!is.null(names(value))) {
      value <- value[order(names(value))]
    }
    return(value)
  }
  if (is.list(value)) {
    value <- lapply(value, .stabilizeOperationIdentity)
    if (!is.null(names(value)) && all(nzchar(names(value)))) {
      value <- value[order(names(value))]
    }
  }
  value
}

.getModuleSpecification <- function(analysisSpecifications, moduleName) {
  index <- which(vapply(
    analysisSpecifications$moduleSpecifications,
    function(specification) tolower(specification$module) == tolower(moduleName),
    logical(1)
  ))
  if (length(index) != 1) {
    stop("Expected exactly one specification for module: ", moduleName, call. = FALSE)
  }
  analysisSpecifications$moduleSpecifications[[index]]
}

.computeOperationChecksum <- function(identity) {
  identity <- .stabilizeOperationIdentity(identity)
  identityJson <- ParallelLogger::convertSettingsToJson(identity)
  unname(CohortGenerator::computeChecksum(identityJson)[[1]])
}

.createTaskInformation <- function(analysisSpecifications, moduleName) {
  moduleSpecification <- .getModuleSpecification(analysisSpecifications, moduleName)
  packageName <- .getModulePackageName(moduleName)
  identity <- list(
    moduleName = moduleName,
    modulePackage = packageName,
    modulePackageVersion = .getInstalledPackageVersion(packageName),
    settings = moduleSpecification$settings,
    sharedResources = analysisSpecifications$sharedResources,
    strategusVersion = .getStrategusVersion()
  )
  identity <- .stabilizeOperationIdentity(identity)
  list(
    taskChecksum = .computeOperationChecksum(identity),
    moduleName = identity$moduleName,
    modulePackage = identity$modulePackage,
    modulePackageVersion = identity$modulePackageVersion,
    strategusVersion = identity$strategusVersion
  )
}

.createUploadDestinationIdentity <- function(resultsDataModelSettings, resultsConnectionDetails) {
  server <- resultsConnectionDetails$server
  if (is.function(server)) {
    server <- server()
  }
  list(
    dbms = tolower(resultsConnectionDetails$dbms),
    server = server,
    resultsDatabaseSchema = resultsDataModelSettings$resultsDatabaseSchema
  )
}

.createOperationInformation <- function(operation, taskInformation,
                                        resultsDataModelSettings = NULL,
                                        resultsConnectionDetails = NULL) {
  checkmate::assertChoice(operation, .SUPPORTED_OPERATIONS)
  if (operation == "EXECUTION") {
    operationChecksum <- taskInformation$taskChecksum
  } else {
    checkmate::assertClass(resultsDataModelSettings, "ResultsDataModelSettings")
    checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails")
    operationChecksum <- .computeOperationChecksum(list(
      taskChecksum = taskInformation$taskChecksum,
      destination = .createUploadDestinationIdentity(
        resultsDataModelSettings = resultsDataModelSettings,
        resultsConnectionDetails = resultsConnectionDetails
      )
    ))
  }
  c(taskInformation, list(operation = operation, operationChecksum = operationChecksum))
}

.operationStatusPath <- function(resultsFolder, moduleName, operation) {
  checkmate::assertChoice(operation, .SUPPORTED_OPERATIONS)
  fileName <- switch(
    operation,
    EXECUTION = "execution_status.json",
    UPLOAD = "upload_status.json"
  )
  file.path(resultsFolder, moduleName, fileName)
}

.formatOperationTime <- function(value) {
  if (is.null(value) || is.na(value)) NULL else format(value, tz = "UTC", usetz = TRUE)
}

.writeOperationStatus <- function(recordPath, operationInformation, state, startTime,
                                  endTime = NULL, elapsedSeconds = NULL, error = NULL) {
  checkmate::assertChoice(state, c("RUNNING", "COMPLETED", "FAILED"))
  dir.create(dirname(recordPath), recursive = TRUE, showWarnings = FALSE)
  record <- list(
    schemaVersion = .OPERATION_STATUS_SCHEMA_VERSION,
    operation = operationInformation$operation,
    taskChecksum = operationInformation$taskChecksum,
    operationChecksum = operationInformation$operationChecksum,
    checksumAlgorithm = .OPERATION_CHECKSUM_ALGORITHM,
    moduleName = operationInformation$moduleName,
    modulePackage = operationInformation$modulePackage,
    modulePackageVersion = operationInformation$modulePackageVersion,
    strategusVersion = operationInformation$strategusVersion,
    state = state,
    startTime = .formatOperationTime(startTime),
    endTime = .formatOperationTime(endTime),
    elapsedSeconds = elapsedSeconds,
    errorClass = if (is.null(error)) NULL else class(error)[[1]],
    errorMessage = if (is.null(error)) NULL else conditionMessage(error)
  )
  class(record) <- "OperationStatusRecord"
  temporaryPath <- tempfile(pattern = "operation-status-", tmpdir = dirname(recordPath), fileext = ".json")
  on.exit(unlink(temporaryPath), add = TRUE)
  ParallelLogger::saveSettingsToJson(record, temporaryPath)
  if (!file.rename(temporaryPath, recordPath)) {
    unlink(recordPath)
    if (!file.rename(temporaryPath, recordPath)) {
      stop("Could not replace operation status record: ", recordPath, call. = FALSE)
    }
  }
  invisible(record)
}

.readOperationStatus <- function(recordPath) {
  tryCatch(ParallelLogger::loadSettingsFromJson(recordPath), error = function(error) NULL)
}

.interpretOperationStatus <- function(recordPath, operationInformation) {
  if (!file.exists(recordPath)) {
    return(list(state = "NOT_STARTED", record = NULL, checksumMatches = NA))
  }
  record <- .readOperationStatus(recordPath)
  requiredNames <- c(
    "schemaVersion", "operation", "taskChecksum", "operationChecksum",
    "checksumAlgorithm", "moduleName", "modulePackage", "modulePackageVersion",
    "strategusVersion", "state"
  )
  isScalarCharacter <- function(value) is.character(value) && length(value) == 1 && !is.na(value)
  schemaVersion <- if (is.list(record)) {
    tryCatch(suppressWarnings(as.integer(record$schemaVersion)), error = function(error) NA_integer_)
  } else {
    NA_integer_
  }
  characterFields <- setdiff(requiredNames, "schemaVersion")
  if (!inherits(record, "OperationStatusRecord") || !all(requiredNames %in% names(record)) ||
      length(schemaVersion) != 1 || is.na(schemaVersion) ||
      schemaVersion != .OPERATION_STATUS_SCHEMA_VERSION ||
      !all(vapply(record[characterFields], isScalarCharacter, logical(1))) ||
      record$checksumAlgorithm != .OPERATION_CHECKSUM_ALGORITHM ||
      record$operation != operationInformation$operation ||
      tolower(record$moduleName) != tolower(operationInformation$moduleName) ||
      !record$state %in% c("RUNNING", "COMPLETED", "FAILED")) {
    return(list(state = "INVALID", record = record, checksumMatches = NA))
  }
  checksumMatches <- identical(record$operationChecksum, operationInformation$operationChecksum)
  if (!checksumMatches) {
    return(list(state = "STALE", record = record, checksumMatches = FALSE))
  }
  list(state = record$state, record = record, checksumMatches = TRUE)
}

.recordValue <- function(record, name, default = NA) {
  value <- if (is.null(record)) NULL else record[[name]]
  if (is.null(value) || length(value) != 1 || is.list(value)) default else value
}

.selectStatusModules <- function(analysisSpecifications, modules) {
  availableModules <- vapply(analysisSpecifications$moduleSpecifications, `[[`, character(1), "module")
  if (!length(availableModules)) {
    stop("The analysis specification contains no modules.", call. = FALSE)
  }
  if (is.null(modules) || !length(modules)) {
    return(availableModules)
  }
  indexes <- match(tolower(modules), tolower(availableModules))
  if (anyNA(indexes)) {
    stop("Unknown modules: ", paste(modules[is.na(indexes)], collapse = ", "), call. = FALSE)
  }
  availableModules[indexes]
}

.getOperationStatus <- function(operation, analysisSpecifications, resultsFolder, modules = NULL,
                                resultsDataModelSettings = NULL,
                                resultsConnectionDetails = NULL) {
  selectedModules <- .selectStatusModules(analysisSpecifications, modules)
  resultsFolder <- normalizePath(resultsFolder, mustWork = FALSE)
  moduleRows <- lapply(selectedModules, function(moduleName) {
    taskInformation <- .createTaskInformation(analysisSpecifications, moduleName)
    operationInformation <- .createOperationInformation(
      operation = operation,
      taskInformation = taskInformation,
      resultsDataModelSettings = resultsDataModelSettings,
      resultsConnectionDetails = resultsConnectionDetails
    )
    recordPath <- .operationStatusPath(resultsFolder, moduleName, operation)
    interpreted <- .interpretOperationStatus(recordPath, operationInformation)
    record <- interpreted$record
    data.frame(
      moduleName = moduleName,
      operation = operation,
      state = interpreted$state,
      expectedTaskChecksum = taskInformation$taskChecksum,
      recordedTaskChecksum = .recordValue(record, "taskChecksum"),
      expectedOperationChecksum = operationInformation$operationChecksum,
      recordedOperationChecksum = .recordValue(record, "operationChecksum"),
      checksumMatches = interpreted$checksumMatches,
      modulePackage = .recordValue(record, "modulePackage", taskInformation$modulePackage),
      modulePackageVersion = .recordValue(record, "modulePackageVersion"),
      strategusVersion = .recordValue(record, "strategusVersion"),
      startTime = .recordValue(record, "startTime"),
      endTime = .recordValue(record, "endTime"),
      elapsedSeconds = .recordValue(record, "elapsedSeconds", NA_real_),
      errorClass = .recordValue(record, "errorClass"),
      errorMessage = .recordValue(record, "errorMessage"),
      recordPath = recordPath,
      stringsAsFactors = FALSE
    )
  })
  moduleStatus <- do.call(rbind, moduleRows)
  aggregateStatus <- if (all(moduleStatus$state == "COMPLETED")) {
    "COMPLETED"
  } else if (any(moduleStatus$state == "FAILED")) {
    "FAILED"
  } else if (any(moduleStatus$state == "RUNNING")) {
    "RUNNING"
  } else {
    "INCOMPLETE"
  }
  result <- list(
    operation = operation,
    status = aggregateStatus,
    modules = moduleStatus,
    resultsFolder = resultsFolder,
    inspectionTime = .formatOperationTime(Sys.time())
  )
  class(result) <- "OperationStatus"
  result
}

#' Inspect execution status
#'
#' Inspects module execution status without executing the analysis or connecting
#' to a database.
#'
#' @template analysisSpecifications
#' @template resultsFolder
#' @param modules Optional character vector selecting modules from the analysis
#' specification. Matching is case-insensitive.
#'
#' @return An `OperationStatus` object.
#' @export
getExecutionStatus <- function(analysisSpecifications, resultsFolder, modules = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertCharacter(resultsFolder, len = 1, add = errorMessages)
  checkmate::assertCharacter(modules, null.ok = TRUE, any.missing = FALSE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  .getOperationStatus("EXECUTION", analysisSpecifications, resultsFolder, modules)
}

#' Inspect upload status
#'
#' Inspects destination-specific module upload status without connecting to the
#' results database.
#'
#' @template analysisSpecifications
#' @template resultsDataModelSettings
#' @template resultsConnectionDetails
#' @param modules Optional character vector selecting modules from the analysis
#' specification. Matching is case-insensitive.
#'
#' @return An `OperationStatus` object.
#' @export
getUploadStatus <- function(analysisSpecifications, resultsDataModelSettings,
                            resultsConnectionDetails, modules = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(resultsDataModelSettings, "ResultsDataModelSettings", add = errorMessages)
  checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails", add = errorMessages)
  checkmate::assertCharacter(modules, null.ok = TRUE, any.missing = FALSE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  .getOperationStatus(
    operation = "UPLOAD",
    analysisSpecifications = analysisSpecifications,
    resultsFolder = resultsDataModelSettings$resultsFolder,
    modules = modules,
    resultsDataModelSettings = resultsDataModelSettings,
    resultsConnectionDetails = resultsConnectionDetails
  )
}
