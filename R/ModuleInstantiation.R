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

#' Ensure all modules are instantiated
#'
#' @description
#' Ensure that all modules referenced in the analysis specifications are instantiated
#' locally in the folder specified in the `INSTANTIATED_MODULES_FOLDER` environmental
#' variable.
#'
#' Missing modules will be fetched from remote repositories.
#'
#' This function will also check whether there are different versions of the same
#' module specified, which is not allowed, and whether all modules required by the
#' specified modules are also instantiated.
#'
#' @template AnalysisSpecifications
#'
#' @return
#' A tibble listing the instantiated modules.
#'
#' @export
ensureAllModulesInstantiated <- function(analysisSpecifications) {
  modules <- getModuleTable(analysisSpecifications, distinct = TRUE)

  # Verify only one version per module:
  multipleVersionsPerModule <- modules %>%
    group_by(.data$module) %>%
    summarise(versions = n()) %>%
    filter(.data$versions > 1)
  if (nrow(multipleVersionsPerModule) > 0) {
    stop(sprintf(
      "Only one version per module allowed in a single analyses specification.\nMultiple versions found for module(s) `%s`.",
      paste(multipleVersionsPerModule$module, collapse = "', '")
    ))
  }

  # Ensure all required modules are instantiated:
  for (i in 1:nrow(modules)) {
    ensureModuleInstantiated(
      module = modules$module[i],
      version = modules$version[i],
      remoteRepo = modules$remoteRepo[i],
      remoteUsername = modules$remoteUsername[i]
    )
  }

  # Check required dependencies have been installed:
  dependencies <- extractDependencies(modules)
  missingDependencies <- dependencies %>%
    filter(!.data$dependsOn %in% modules$module)
  if (nrow(missingDependencies) > 0) {
    message <- paste(
      c(
        "Detected missing dependencies:",
        sprintf("- Missing module '%s' required by module '%s'", missingDependencies$dependsOn, missingDependencies$module)
      ),
      collapse = "\n"
    )
    stop(message)
  }

  return(modules)
}

getModuleTable <- function(analysisSpecifications, distinct = FALSE) {
  modules <- lapply(
    analysisSpecifications$moduleSpecifications,
    function(x) {
      tibble(
        module = x$module,
        version = x$version,
        remoteRepo = x$remoteRepo,
        remoteUsername = x$remoteUsername
      )
    }
  ) %>%
    bind_rows()
  if (distinct) {
    modules <- modules %>%
      distinct(.data$module, .data$version, .keep_all = TRUE)
  }
  return(modules)
}

extractDependencies <- function(modules) {
  extractDependenciesSingleModule <- function(module) {
    moduleFolder <- getModuleFolder(module$module, module$version)
    metaData <- getModuleMetaData(moduleFolder)
    dependencies <- tibble(
      module = module$module,
      dependsOn = as.character(metaData$Dependencies)
    )
    return(dependencies)
  }
  dependencies <- lapply(split(modules, 1:nrow(modules)), extractDependenciesSingleModule) %>%
    bind_rows()
  return(dependencies)
}

getModuleMetaData <- function(moduleFolder) {
  jsonFileName <- file.path(moduleFolder, "MetaData.json")
  if (!file.exists(jsonFileName)) {
    stop(sprintf("Meta-data JSON not found in '%s'.", moduleFolder))
  }
  metaData <- ParallelLogger::loadSettingsFromJson(jsonFileName)
  return(metaData)
}

getModuleFolder <- function(module, version) {
  moduleFolder <- file.path(Sys.getenv("INSTANTIATED_MODULES_FOLDER"), sprintf("%s_%s", module, version))
  invisible(moduleFolder)
}

ensureModuleInstantiated <- function(module, version, remoteRepo, remoteUsername) {
  instantiatedModulesFolder <- Sys.getenv("INSTANTIATED_MODULES_FOLDER")
  if (instantiatedModulesFolder == "") {
    stop("The INSTANTIATED_MODULES_FOLDER environment variable has not been set.")
  }
  if (!dir.exists(instantiatedModulesFolder)) {
    dir.create(instantiatedModulesFolder, recursive = TRUE)
  }
  moduleFolder <- getModuleFolder(module, version)
  if (!dir.exists(moduleFolder)) {
    instantiateModule(module, version, remoteRepo, remoteUsername, moduleFolder)
  }
  return(moduleFolder)
}

instantiateModule <- function(module, version, remoteRepo, remoteUsername, moduleFolder) {
  dir.create(moduleFolder)
  success <- FALSE
  on.exit(if (!success) unlink(moduleFolder, recursive = TRUE))
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
    moduleFile <- file.path(moduleFolder, sprintf("%s_%s.zip", module, version))
    moduleUrl <- sprintf("https://%s/%s/%s/archive/refs/tags/v%s.zip", remoteRepo, remoteUsername, module, version)
    utils::download.file(url = moduleUrl, destfile = moduleFile)
    utils::unzip(zipfile = moduleFile, exdir = moduleFolder)
    unlink(moduleFile)
    # At this point, the unzipped folders will likely exist in a sub folder.
    # Move all files from that sub folder to the main module folder
    subFolders <- list.dirs(path = moduleFolder, recursive = FALSE)
    if (length(subFolders) > 0) {
      for (i in 1:length(subFolders)) {
        R.utils::copyDirectory(
          from = subFolders[i],
          to = moduleFolder,
          recursive = TRUE
        )
        unlink(subFolders[i], recursive = TRUE)
      }
    }
  }

  # Verify the structure of the module to ensure that
  # it contains the proper files required by renv
  # before we restore from the renv.lock file
  renvDependencies <- getModuleRenvDependencies(moduleFolder)
  if (nrow(renvDependencies) > 0) {
    message <- paste(
      c(
        sprintf("The module '%s' (v%s) is missing the following files required by renv:", module, version),
        sprintf("- Missing renv dependency '%s'", renvDependencies$fileName),
        "As a result, Strategus cannot use this module as part of the execution pipeline otherwise it may corrupt your R library.",
        "Please check to see if a newer version of this module exists and update your analysis specification to use that module instead."
      ),
      collapse = "\n"
    )
    stop(message)
  }

  script <- paste(
    c(
      "renv::install(c('ParallelLogger', 'keyring'), prompt = FALSE)",
      sprintf("ParallelLogger::addDefaultFileLogger(file.path('%s', 'moduleInitLog.txt'))", moduleFolder),
      sprintf("ParallelLogger::addDefaultErrorReportLogger(fileName = file.path('%s', 'moduleInitErrorReport.R'))", moduleFolder),
      "renv::restore(prompt = FALSE)"
     ),
    collapse = "\n"
  )
  tempScriptFile <- tempfile(fileext = ".R")
  fileConn <- file(tempScriptFile)
  writeLines(script, fileConn)
  close(fileConn)

  renv::run(
    script = tempScriptFile,
    job = FALSE,
    name = "Buidling renv library",
    project = moduleFolder
  )
  success <- TRUE
}

getModuleRenvDependencies <- function(moduleFolder) {
  renvRequiredFiles <- c(
    ".Rprofile",
    "renv.lock",
    "renv/activate.R",
    "renv/settings.dcf"
  )

  missingFiles <- tibble::enframe(renvRequiredFiles) %>%
    dplyr::mutate(fileExists = file.exists(file.path(moduleFolder, .data$value))) %>%
    dplyr::rename(fileName = .data$value) %>%
    dplyr::select("fileName", "fileExists") %>%
    dplyr::filter(.data$fileExists == FALSE)

  invisible(missingFiles)
}

getModuleTablePrefixes <- function(moduleList) {
  moduleTablePrefix <- tibble::tibble()
  for (i in 1:nrow(moduleList)) {
    moduleMetaData <- getModuleMetaData(
      moduleFolder = getModuleFolder(
        module = moduleList$module[i],
        version = moduleList$version[i]
      )
    )
    moduleTablePrefix <- moduleTablePrefix %>%
      bind_rows(tibble::tibble(
        moduleName = moduleList$module[i],
        moduleVersion = moduleList$version[i],
        tablePrefix = moduleMetaData$TablePrefix
      ))
  }

  invisible(moduleTablePrefix)
}
