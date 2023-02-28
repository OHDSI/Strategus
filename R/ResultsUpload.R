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


#' Results upload callbacks for inserting results in to a database
runResultsUpload <- function(analysisSpecifications, keyringSettings, moduleIndex, executionSettings, ...) {
  checkmate::assert_multi_class(x = executionSettings, classes = c("ResultsExecutionSettings"))
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


  if (!dir.exists(moduleExecutionSettings$resultsSubFolder)) {
    stop("reulsts not found")
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
    uploadResultsCallback <- NULL
    source('Main.R')
    moduleInfo <- ParallelLogger::loadSettingsFromJson('MetaData.json')
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
      "resultsConnectionDetails <- keyring::key_get(jobContext$moduleExecutionSettings$resultsConnectionDetailsReference, keyring = keyringName)
       resultsConnectionDetails <- ParallelLogger::convertJsonToSettings(resultsConnectionDetails)
       resultsConnectionDetails <- do.call(DatabaseConnector::createConnectionDetails, resultsConnectionDetails)
       jobContext$moduleExecutionSettings$resultsConnectionDetails <- resultsConnectionDetails"
    )
  } else {
    stop("Unhandled executionSettings class! Must be CdmExecutionSettings instance")
  }

  script <- paste0(script, "
    if (keyringLocked) {
       keyring::keyring_lock(keyring = keyringName)
    }

    ParallelLogger::addDefaultFileLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'log.txt'))
    ParallelLogger::addDefaultErrorReportLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'errorReport.R'))

    if (Sys.getenv('FORCE_RENV_USE', '') == 'TRUE') {
      renv::use(lockfile = 'renv.lock')
    }

    # Override default behaviour and do module specific upload
    if (is.function(uploadResultsCallback)) {
      uploadResultsCallback(jobContext)
    } else {
      ResultModelManager::uploadResults(connectionDetails = jobContext$moduleExecutionSettings$resultsConnectionDetails,
                                        schema = jobContext$moduleExecutionSettings$resultsDatabaseSchema,
                                        resultsFolder = jobContext$moduleExecutionSettings$resultsSubFolder,
                                        tablePrefix = moduleInfo$TablePrefix,
                                        forceOverWriteOfSpecifications = FALSE,
                                        purgeSiteDataBeforeUploading = TRUE,
                                        databaseIdentifierFile = 'database_meta_data.csv',
                                        runCheckAndFixCommands = FALSE,
                                        warnOnMissingTable = TRUE,
                                        specifications)
    }

    ParallelLogger::unregisterLogger('DEFAULT_FILE_LOGGER', silent = TRUE)
    ParallelLogger::unregisterLogger('DEFAULT_ERRORREPORT_LOGGER', silent = TRUE)
    writeLines('results.uploaded', file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'results.uploaded'))
    ")

  script <- gsub("jobContextFileName", sprintf("\"%s\"", jobContextFileName), script)
  tempScriptFile <- jobContextFileName <- file.path(moduleExecutionSettings$workSubFolder, "StrategusResultsScript.R") # gsub("\\\\", "/", tempfile(fileext = ".R"))
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
    name = "Running results upload",
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
