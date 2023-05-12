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


checkModuleDependencies <- function(dependencies, moduleSet) {

  for (dep in dependencies) {
    dp <- strsplit(dep, split = " ")[[1]]

    if (!dp[1] %in% moduleSet$moduleName) {
      stop("Dependent module not installed - ", dp)
    }

    # TODO: Check version
  }
}


#' List currently downloaded module cache
#' @description
#' This returns the set of currently installed in the specifed system path
#'
#' @export
getAvailableModules <- function(installedModulesPath = Sys.getenv("STRATEGUS_INSTALLED_MODULES"),
                                refreshCache = FALSE) {

  if (installedModulesPath == "") {
    warning("No system module path set, set STRATEGUS_INSTALLED_MODULES in your .renviron to enable global modules")
  }

  modulesRdsFile <- file.path(installedModulesPath, "installedModules.rds")

  if (!file.exists(modulesRdsFile)) {
    return(data.frame(moduleName = c(),
                      modulePath = c(),
                      dependencies = c(),
                      version = c()))
  }

  return(readRDS(modulesRdsFile))
}

#' Install a module into the cache from local directory
#' @description
#'
#' Modules can be installed but not instantiated. To be instantiated is to have the renv initialised with all required
#' packages installed. This is a required step for running code inside a module but not for creating study configuration
#' files.
#'
#'
installLocalModule <- function(pathToModule,
                               overwrite = FALSE,
                               instantiate = FALSE,
                               installedModulesPath = Sys.getenv("STRATEGUS_INSTALLED_MODULES")) {
  # Validate required module files are present
  requiredFiles <- c(
    "MetaData.json",
    "Main.R",
    "SettingsFunctions.R",
    "renv.lock"
  )

  checkmate::assertFileExists(file.path(pathToModule, requiredFiles))
  # lock rds object
  installLockFile <- file.path(installedModulesPath, "install_module.lock")
  if (file.exists(installLockFile)) {
    ts <- readLines(installLockFile)
    stop("Cannot install module operation is in progress or ", installLockFile, "must be manually deleted.\n", ts)
  }

  # Save a timestamp inside a file to inform when last lock was
  a <- writeLines(timestamp(), con = installLockFile)
  on.exit(unlink(installLockFile, force = TRUE))
  currentModules <- getAvailableModules(installedModulesPath = installedModulesPath)
  metaData <- getModuleMetaData(pathToModule)

  if (nrow(currentModules)) {
    if (!overwrite) {
      installCount <- currentModules %>%
        dplyr::filter(.data$moduleName == metaData$Name,
                      .data$version == metaData$Version) %>%
        dplyr::count() %>%
        dplyr::pull()

      if (installCount != 0)
        stop("Module of same name and version already exists in cache. Use overwrite = TRUE  to reinstall")
    }
    # Check dependencies - any dependent modules must be installed in the cache
    checkModuleDependencies(metaData$Dependencies, currentModules)
  } else if (length(metaData$Dependencies)) {
    stop("Dependent modules not installed - ", metaData$Dependencies)
  }
  installPath <- file.path(installedModulesPath, metaData$Name, metaData$Version)

  if (dir.exists(installPath))
    unlink(installPath, recursive = TRUE, force = TRUE)

  iRow <- tibble::tibble(moduleName = c(metaData$Name),
                         modulePath = c(installPath),
                         dependencies = paste(metaData$Dependencies, collapse = ";"),
                         version = c(metaData$Version))

  fs::dir_copy(pathToModule, installPath, overwrite = TRUE)

  currentModules <- dplyr::bind_rows(currentModules, iRow)
  # Write to install path
  modulesRdsFile <- file.path(installedModulesPath, "installedModules.rds")
  # save rds
  saveRDS(currentModules, modulesRdsFile)

  message(paste("Module", iRow$moduleName, "Installed"))
  invisible(NULL)
}

#' Install a module from github
#'
#'
installGithubModule <- function(repoPath, ref = "main", ...) {
  # Download tarball
  tfile <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tfile))

  download.file(url = paste0("http://github.com/", repoPath, "/tarball/", ref), destfile = tfile)
  tdir <- tempfile()
  on.exit(unlink(tdir, recursive = TRUE), add = TRUE)

  utils::untar(tfile, exdir = tdir)
  finalDir <- file.path(tdir, list.files(tdir)[1])

  installLocalModule(finalDir, ...)
}
