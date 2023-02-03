# Copyright 2023 Observational Health Data Sciences and Informatics
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

runModule <- function(analysisSpecifications, keyringSettings, moduleIndex, executionSettings, ...) {
  checkmate::assert_multi_class(x = executionSettings, classes = c("CdmExecutionSettings", "ResultsExecutionSettings"))
  moduleSpecification <- analysisSpecifications$moduleSpecifications[[moduleIndex]]

  module <- moduleSpecification$module
  version <- moduleSpecification$version
  remoteRepo <- moduleSpecification$remoteRepo
  remoteUsername <- moduleSpecification$remoteUsername
  moduleFolder <- ensureModuleInstantiated(module, version, remoteRepo, remoteUsername)

  # Create job context
  moduleExecutionSettings <- executionSettings
  moduleExecutionSettings$workSubFolder <- file.path(executionSettings$workFolder, sprintf("%s_%d", module, moduleIndex))
  moduleExecutionSettings$resultsSubFolder <- file.path(executionSettings$resultsFolder, sprintf("%s_%d", module, moduleIndex))

  if (!dir.exists(moduleExecutionSettings$workSubFolder)) {
    dir.create(moduleExecutionSettings$workSubFolder, recursive = TRUE)
  }
  if (!dir.exists(moduleExecutionSettings$resultsSubFolder)) {
    dir.create(moduleExecutionSettings$resultsSubFolder, recursive = TRUE)
  }
  jobContext <- list(
    sharedResources = analysisSpecifications$sharedResources,
    settings = moduleSpecification$settings,
    moduleExecutionSettings = moduleExecutionSettings,
    keyringSettings = keyringSettings
  )
  jobContextFileName <- file.path(moduleExecutionSettings$workSubFolder, "jobContext.rds") # gsub("\\\\", "/", tempfile(fileext = ".rds"))
  saveRDS(jobContext, jobContextFileName)

  # Execute module using settings
  script <- "
    #source('renv/activate.R')
    source('Main.R')
    jobContext <- readRDS(jobContextFileName)

    # If the keyring is locked, unlock it, set the value and then re-lock it
    keyringName <- jobContext$keyringSettings$keyringName
    keyringLocked <- Strategus::unlockKeyring(keyringName = keyringName)
  "

  # Set the connection information based on the type of execution being
  # performed
  if (is(executionSettings, "CdmExecutionSettings")) {
    script <- paste0(
      script,
      "connectionDetails <- keyring::key_get(jobContext$moduleExecutionSettings$connectionDetailsReference, keyring = keyringName)
       connectionDetails <- ParallelLogger::convertJsonToSettings(connectionDetails)
       connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, connectionDetails)
       jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails"
    )
  } else if (is(executionSettings, "ResultsExecutionSettings")) {
    script <- paste0(
      script,
      "resultsConnectionDetails <- keyring::key_get(jobContext$moduleExecutionSettings$resultsConnectionDetailsReference, keyring = keyringName)
       resultsConnectionDetails <- ParallelLogger::convertJsonToSettings(resultsConnectionDetails)
       resultsConnectionDetails <- do.call(DatabaseConnector::createConnectionDetails, resultsConnectionDetails)
       jobContext$moduleExecutionSettings$resultsConnectionDetails <- resultsConnectionDetails"
    )
  } else {
    stop("Unhandled executionSettings class! Must be one of the following: CdmExecutionSettings, ResultsExecutionSettings")
  }

  script <- paste0(script, "
    if (keyringLocked) {
       keyring::keyring_lock(keyring = keyringName)
    }

    ParallelLogger::addDefaultFileLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'log.txt'))
    ParallelLogger::addDefaultErrorReportLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'errorReport.R'))

    options(andromedaTempFolder = file.path(jobContext$moduleExecutionSettings$workFolder, 'andromedaTemp'))

    if (Sys.getenv('FORCE_RENV_USE', '') == 'TRUE') {
      renv::use(lockfile = 'renv.lock')
    }
    execute(jobContext)

    ParallelLogger::unregisterLogger('DEFAULT_FILE_LOGGER', silent = TRUE)
    ParallelLogger::unregisterLogger('DEFAULT_ERRORREPORT_LOGGER', silent = TRUE)
    writeLines('done', file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'done'))
    ")

  script <- gsub("jobContextFileName", sprintf("\"%s\"", jobContextFileName), script)
  tempScriptFile <- jobContextFileName <- file.path(moduleExecutionSettings$workSubFolder, "StrategusScript.R") # gsub("\\\\", "/", tempfile(fileext = ".R"))
  fileConn <- file(tempScriptFile)
  writeLines(script, fileConn)
  close(fileConn)

  doneFile <- file.path(jobContext$moduleExecutionSettings$resultsSubFolder, "done")
  if (file.exists(doneFile)) {
    unlink(doneFile)
  }
  renv::run(
    script = tempScriptFile,
    job = FALSE,
    name = "Running module",
    project = moduleFolder
  )
  if (!file.exists(doneFile)) {
    message <- paste(
      "Module did not complete. To debug:",
      sprintf("  rstudioapi::openProject('%s', newSession = TRUE)", moduleFolder),
      sprintf("  file.edit('%s')", tempScriptFile),
      sep = "\n"
    )
    stop(message)
  }

  return(list(dummy = 123))
}
