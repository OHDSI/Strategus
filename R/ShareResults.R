#' Create a zip file with all study results for sharing with study coordinator
#'
#' @details
#' Creates a `.zip` file of the `.csv` files found in the
#' `resultsFolder`. The resulting `.zip` file will have
#' relative paths to the root of the `resultsFolder`
#' which is generally found in `executionSettings$resultsFolder`.
#'
#' The `.zip` file also contains an `execution_summary.json` file that
#' summarizes the `execution_status.json` record of each module found in the
#' `resultsFolder`: the module state, task checksum, package versions, timing
#' and, for completed modules, the path, SHA-256 checksum and size of each
#' `.csv` file the module wrote. A study coordinator can use it to confirm
#' which modules completed and that the shared files are the ones each
#' completed module produced. Error messages are not included because they
#' may contain server names or file paths; only the error class is kept.
#'
#' @template resultsFolder
#'
#' @param zipFile The path to the zip file to be created.
#'
#' @return
#' Does not return anything. Is called for the side-effect of creating the
#' zip file with results.
#'
#' @export
zipResults <- function(resultsFolder, zipFile) {
  files <- list.files(
    path = resultsFolder,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  summaryPath <- file.path(resultsFolder, .EXECUTION_SUMMARY_FILE_NAME)
  .writeExecutionSummary(resultsFolder, summaryPath)
  on.exit(unlink(summaryPath), add = TRUE)
  files <- c(files, summaryPath)

  oldWd <- setwd(resultsFolder)
  on.exit(setwd(oldWd), add = TRUE)
  DatabaseConnector::createZipFile(
    zipFile = zipFile,
    files = files,
    rootFolder = resultsFolder
  )
  message(zipFile, " created.")
}

.EXECUTION_SUMMARY_FILE_NAME <- "execution_summary.json"
.EXECUTION_SUMMARY_SCHEMA_VERSION <- 1L

# Fields copied from each execution_status.json record. errorMessage is left
# out on purpose: it can contain server names, schema names or local paths.
.EXECUTION_SUMMARY_FIELDS <- c(
  "state", "taskChecksum", "operationChecksum", "checksumAlgorithm",
  "modulePackage", "modulePackageVersion", "strategusVersion",
  "startTime", "endTime", "elapsedSeconds", "errorClass", "outputFiles"
)

.createExecutionSummary <- function(resultsFolder) {
  recordPaths <- sort(list.files(
    path = resultsFolder,
    pattern = "^execution_status\\.json$",
    recursive = TRUE,
    full.names = TRUE
  ))
  modules <- lapply(recordPaths, function(recordPath) {
    moduleName <- basename(dirname(recordPath))
    record <- .readOperationStatus(recordPath)
    if (!inherits(record, "OperationStatusRecord")) {
      return(list(moduleName = moduleName, state = "INVALID"))
    }
    if (!is.null(record$moduleName)) {
      moduleName <- record$moduleName
    }
    fields <- intersect(.EXECUTION_SUMMARY_FIELDS, names(record))
    c(list(moduleName = moduleName), unclass(record)[fields])
  })
  list(
    schemaVersion = .EXECUTION_SUMMARY_SCHEMA_VERSION,
    outputFileChecksumAlgorithm = .OUTPUT_FILE_CHECKSUM_ALGORITHM,
    strategusVersion = .getStrategusVersion(),
    createdTime = .formatOperationTime(Sys.time()),
    modules = modules
  )
}

.writeExecutionSummary <- function(resultsFolder, summaryPath) {
  summary <- .createExecutionSummary(resultsFolder)
  ParallelLogger::saveSettingsToJson(summary, summaryPath)
  invisible(summary)
}
