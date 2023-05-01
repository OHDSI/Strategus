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


# Results upload callbacks for inserting results in to a database
runResultsUpload <- function(analysisSpecifications, keyringSettings, moduleIndex, executionSettings, ...) {
  checkmate::assert_multi_class(x = executionSettings, classes = c("ExecutionSettings"))
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

  if (!is(executionSettings, "CdmExecutionSettings")) {
    stop("Unhandled executionSettings class! Must be CdmExecutionSettings instance")
  }

  if (!dir.exists(moduleExecutionSettings$resultsSubFolder)) {
    stop("results not found")
  }
  jobContext <- list(
    sharedResources = analysisSpecifications$sharedResources,
    settings = moduleSpecification$settings,
    moduleExecutionSettings = moduleExecutionSettings,
    keyringSettings = keyringSettings
  )
  jobContextFileName <- file.path(moduleExecutionSettings$workSubFolder, "jobContext.rds") # gsub("\\\\", "/", tempfile(fileext = ".rds"))
  saveRDS(jobContext, jobContextFileName)
  dataModelExportPath <- file.path(moduleExecutionSettings$workSubFolder, "resultsDataModelSpecification.csv")

  doneFile <- file.path(jobContext$moduleExecutionSettings$resultsSubFolder, "results.uploaded")
  if (file.exists(doneFile)) {
    unlink(doneFile)
  }

  tempScriptFile <- file.path(moduleExecutionSettings$workSubFolder, "UploadScript.R")

  ##
  # Module space executed code
  ##
  withModuleRenv({
    uploadResultsCallback <- NULL

    getDataModelSpecifications <- function(...) {
      ParallelLogger::logInfo("Getting result model specification")
      if (file.exists('resultsDataModelSpecification.csv')) {
        res <- readr::read_csv('resultsDataModelSpecification.csv', show_col_types = FALSE)
        return(res)
      }
      ParallelLogger::logInfo("No result model specification found")
      return(NULL)
    }
    source('Main.R')
    moduleInfo <- ParallelLogger::loadSettingsFromJson('MetaData.json')
    jobContext <- readRDS(jobContextFileName)
    specifications <- getDataModelSpecifications(jobContext)
    ParallelLogger::addDefaultFileLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'log.txt'))
    ParallelLogger::addDefaultErrorReportLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'errorReportR.txt'))

    if (Sys.getenv('FORCE_RENV_USE', '') == 'TRUE') {
      renv::use(lockfile = 'renv.lock')
    }

    # Override default behaviour and do module specific upload inside module context?
    if (is.function(uploadResultsCallback)) {
      ParallelLogger::logInfo("Calling module result upload functionality")
      # If the keyring is locked, unlock it, set the value and then re-lock it
      ParallelLogger::logInfo("-- Getting result database credentials")
      keyringName <- jobContext$keyringSettings$keyringName
      keyringLocked <- Strategus::unlockKeyring(keyringName = keyringName)
      resultsConnectionDetails <- keyring::key_get(jobContext$moduleExecutionSettings$resultsConnectionDetailsReference, keyring = keyringName)
      resultsConnectionDetails <- ParallelLogger::convertJsonToSettings(resultsConnectionDetails)
      resultsConnectionDetails <- do.call(DatabaseConnector::createConnectionDetails, resultsConnectionDetails)
      jobContext$moduleExecutionSettings$resultsConnectionDetails <- resultsConnectionDetails
      ParallelLogger::logInfo("-- Executing upload callback")
      uploadResultsCallback(jobContext)
      if (keyringLocked) {
        keyring::keyring_lock(keyring = keyringName)
      }
      ParallelLogger::logInfo("-- Upload completed")
      writeLines('results.uploaded', doneFile)
    } else if (is.null(specifications)) {
      ParallelLogger::logInfo("No result specifications found, assuming module has produced no results")
      # NO spect file Status
      warning('data model specifications not loaded from module - skipping results upload')
      writeLines('no.spec.found', doneFile)
    } else {
      # Spec file written
      ParallelLogger::logInfo("Writing spec for result upload outside of module context")
      readr::write_csv(specifications, dataModelExportPath)
      writeLines('specifications.written', doneFile)
    }

    ParallelLogger::unregisterLogger('DEFAULT_FILE_LOGGER', silent = TRUE)
    ParallelLogger::unregisterLogger('DEFAULT_ERRORREPORT_LOGGER', silent = TRUE)

  },
    moduleFolder = moduleFolder,
    tempScriptFile = tempScriptFile,
    injectVars = list(jobContextFileName = jobContextFileName,
                      dataModelExportPath = dataModelExportPath,
                      doneFile = doneFile)
  )
  ##
  # end Module executed code
  ##
  if (!file.exists(doneFile)) {
    message <- paste(
      "Module did not complete. To debug:",
      sprintf("  rstudioapi::openProject('%s', newSession = TRUE)", moduleFolder),
      sprintf("  file.edit('%s')", tempScriptFile),
      sep = "\n"
    )
    stop(message)
  }

  workStatus <- readLines(doneFile)

  if (workStatus == 'specifications.written') {
    ParallelLogger::logInfo("Uploading results according to module specification")
    specifications <- readr::read_csv(dataModelExportPath, show_col_types = FALSE)
    colnames(specifications) <- SqlRender::snakeCaseToCamelCase(colnames(specifications))
    moduleInfo <- ParallelLogger::loadSettingsFromJson(file.path(moduleFolder, 'MetaData.json'))

    keyringName <- jobContext$keyringSettings$keyringName
    keyringLocked <- Strategus::unlockKeyring(keyringName = keyringName)

    ParallelLogger::logInfo("Getting result database credentials")
    resultsConnectionDetails <- keyring::key_get(jobContext$moduleExecutionSettings$resultsConnectionDetailsReference, keyring = keyringName)
    resultsConnectionDetails <- ParallelLogger::convertJsonToSettings(resultsConnectionDetails)
    resultsConnectionDetails <- do.call(DatabaseConnector::createConnectionDetails, resultsConnectionDetails)
    jobContext$moduleExecutionSettings$resultsConnectionDetails <- resultsConnectionDetails

    ParallelLogger::logInfo("Calling RMM for upload")
    ResultModelManager::uploadResults(connectionDetails = jobContext$moduleExecutionSettings$resultsConnectionDetails,
                                      schema = jobContext$moduleExecutionSettings$resultsDatabaseSchema,
                                      resultsFolder = jobContext$moduleExecutionSettings$resultsSubFolder,
                                      tablePrefix = moduleInfo$TablePrefix,
                                      forceOverWriteOfSpecifications = FALSE,
                                      purgeSiteDataBeforeUploading = FALSE,
                                      databaseIdentifierFile = 'database_meta_data.csv',
                                      runCheckAndFixCommands = FALSE,
                                      warnOnMissingTable = TRUE,
                                      specifications = specifications)

    ParallelLogger::logInfo("Upload completed")
    if (keyringLocked) {
      keyring::keyring_lock(keyring = keyringName)
    }
  } else if (workStatus == 'results.uploaded')  {
    message("Result upload handled inside module execution envrionment")
  } else {
    message("Results not uploaded for module")
  }

  return(list(dummy = 123))
}
