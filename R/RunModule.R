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
  moduleFolder <- ensureModuleInstantiated(module, version)

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
            name = "Running module",
            project = moduleFolder)

  return(list(dummy = 123))
}

ensureModuleInstantiated <- function(module, version) {
  instantiatedModulesFolder <- Sys.getenv("INSTANTIATED_MODULES_FOLDER")
  if (!dir.exists(instantiatedModulesFolder)) {
    dir.create(instantiatedModulesFolder, recursive = TRUE)
  }
  moduleFolder <- file.path(instantiatedModulesFolder, sprintf("%s_v%s", module, version))
  if (!dir.exists(moduleFolder)) {
    instantiateModule(module, version, moduleFolder)
  }
  return(moduleFolder)
}

instantiateModule <- function(module, version, moduleFolder) {
  # todo: make sure folder is deleted if instantiation fails
  dir.create(moduleFolder)

  if (module == "TestModule1") {
    # For demo purposes only: get module from extras folder
    files <- list.files("extras/TestModules/TestModule1", full.names = TRUE, include.dirs = TRUE, all.files = TRUE)
    files <- files[!grepl("renv$", files)]
    files <- files[!grepl("\\.$", files)]
    files <- files[!grepl(".Rhistory$", files)]
    file.copy(files, moduleFolder, recursive = TRUE)
    dir.create(file.path(moduleFolder, "renv"))
    file.copy("extras/TestModules/TestModule1/renv/activate.R", file.path(moduleFolder, "renv"), recursive = TRUE)
  } else {
    # todo: get module (from GitHub)
  }

  script <- "
    renv::restore(prompt = FALSE)
    if (!require('ParallelLogger', quietly = TRUE)) {
      install.packages('ParallelLogger')
    }
    if (!require('keyring', quietly = TRUE)) {
      install.packages('keyring')
    }
  "
  tempScriptFile <- tempfile(fileext = ".R")
  fileConn<-file(tempScriptFile)
  writeLines(script, fileConn)
  close(fileConn)

  renv::run(script = tempScriptFile,
            name = "Buidling renv library",
            project = moduleFolder)

}

