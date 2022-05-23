# Copyright 2022 Observational Health Data Sciences and Informatics
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

runModule <- function(analysisSpecifications, moduleIndex, executionSettings, ...) {

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
  jobContext <- list(sharedResources = analysisSpecifications$sharedResources,
                     settings = moduleSpecification$settings,
                     moduleExecutionSettings = moduleExecutionSettings)

  if (file.exists(file.path(moduleFolder, "renv.lock"))) {
    jobContextFileName <- gsub("\\\\", "/", tempfile(fileext = ".rds"))
    saveRDS(jobContext, jobContextFileName)

    # Execute module using settings
    script <- "
    source('Main.R')
    jobContext <- readRDS(jobContextFileName)
    connectionDetails <- keyring::key_get(jobContext$moduleExecutionSettings$connectionDetailsReference)
    connectionDetails <- ParallelLogger::convertJsonToSettings(connectionDetails)
    connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, connectionDetails)
    jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails

    ParallelLogger::addDefaultFileLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'log.txt'))
    ParallelLogger::addDefaultErrorReportLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'errorReport.R'))

    execute(jobContext)

    ParallelLogger::unregisterLogger('DEFAULT_FILE_LOGGER', silent = TRUE)
    ParallelLogger::unregisterLogger('DEFAULT_ERRORREPORT_LOGGER', silent = TRUE)
  "
    script <- gsub("jobContextFileName", sprintf("\"%s\"", jobContextFileName), script)
    tempScriptFile <- tempfile(fileext = ".R")
    fileConn<-file(tempScriptFile)
    writeLines(script, fileConn)
    close(fileConn)

    renv::run(script = tempScriptFile,
              job = FALSE,
              name = "Running module",
              project = moduleFolder)
  } else {
    # Run in 1-node cluster to avoid making a mess in the local R environment:
    print("check 1")
    cluster <- ParallelLogger::makeCluster(numberOfThreads = 1, singleThreadToMain = FALSE)
    on.exit(ParallelLogger::stopCluster(cluster))
    ParallelLogger::clusterApply(cluster, moduleFolder, runModuleInCluster, jobContext = jobContext)
    print("check 2")
  }
  return(list(dummy = 123))
}

runModuleInCluster <- function(moduleFolder, jobContext) {
  ParallelLogger::addDefaultFileLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'log.txt'))
  ParallelLogger::addDefaultErrorReportLogger(file.path(jobContext$moduleExecutionSettings$resultsSubFolder, 'errorReport.R'))

  setwd(moduleFolder)
  source("Main.R")
  connectionDetails <- keyring::key_get(jobContext$moduleExecutionSettings$connectionDetailsReference)
  connectionDetails <- ParallelLogger::convertJsonToSettings(connectionDetails)
  connectionDetails <- do.call(DatabaseConnector::createConnectionDetails, connectionDetails)
  jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails
  execute(jobContext)

  ParallelLogger::unregisterLogger('DEFAULT_FILE_LOGGER', silent = TRUE)
  ParallelLogger::unregisterLogger('DEFAULT_ERRORREPORT_LOGGER', silent = TRUE)
  return(NULL)
}
