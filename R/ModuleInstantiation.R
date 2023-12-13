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
#' @template forceVerification
#'
#' @return
#' A list containing the install status of all modules
#' (TRUE if all are installed properly) and a tibble listing
#' the instantiated modules.
#'
#' @export
ensureAllModulesInstantiated <- function(analysisSpecifications, forceVerification = FALSE) {
  modules <- getModuleTable(analysisSpecifications, distinct = TRUE)

  # Verify only one version per module:
  multipleVersionsPerModule <- modules %>%
    group_by(module) %>%
    summarise(versions = n()) %>%
    filter(versions > 1)
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
    filter(!dependsOn %in% modules$module)
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

  # Verify all modules are properly installed
  moduleInstallStatus <- list()
  for (i in 1:nrow(modules)) {
    status <- verifyModuleInstallation(
      module = modules$module[i],
      version = modules$version[i],
      forceVerification = forceVerification
    )
    moduleInstallStatus[[length(moduleInstallStatus) + 1]] <- status
  }
  attr(modules, 'moduleInstallStatus') <- moduleInstallStatus

  installStatus <- unlist(lapply(moduleInstallStatus, FUN = function(x) { x$moduleInstalled }))
  if (!all(installStatus)) {
    problemModules <- status[!installStatus]
    message("There were ", length(problemModules), " issue(s) found with your Strategus modules!")
    for (i in seq_along(problemModules)) {
      message("Issue #", i, ": Module ", problemModules[[i]]$moduleFolder, " could not install the following R packages:")
      print(problemModules[[i]]$issues)
    }
    message("To fix these issues, open the module project at the path specified above and re-run \"renv::restore()\" and correct all issues")
  }

  return(
    list(
      allModulesInstalled = all(installStatus),
      modules = modules
    )
  )
}


#' Verify a module is properly installed
#'
#' @description
#' In some instances a module may fail to instantiate and install due to problems
#' when calling renv::restore for the module's renv.lock file. This function
#' will allow you to surface inconsistencies between the module renv.lock file
#' and the module's renv project library. This function will check to that a
#' module has been properly installed using internal functions of the `renv`
#' package. If a module is verified to work via this function, the hash of
#' the module's renv.lock file will be written to a text file in the module
#' directory to indicate that it is ready for use. This will allow subsequent
#' calls to work faster since the initial verification process can take some
#' time.It is possible to re-run the verification of a module
#' by using the `forceVerification` parameter.
#'
#' To fix issues with a module, you will need to open the module's .Rproj in
#' RStudio instance and debug the issues when calling renv::restore().
#'
#' @param module The name of the module to verify (i.e. "CohortGeneratorModule")
#'
#' @param version The version of the module to verify (i.e. "0.2.1")
#'
#' @param silent When TRUE output of this verification process is suppressed
#'
#' @template forceVerification
#'
#' @return
#' A list with the output of the consistency check
#'
#' @export
verifyModuleInstallation <- function(module, version, silent = FALSE, forceVerification = FALSE) {
  # Internal helper function
  verifyModuleInstallationReturnValue <- function(moduleFolder, moduleInstalled, issues = NULL) {
    returnVal <- list(
      moduleFolder = moduleFolder,
      moduleInstalled = moduleInstalled,
      issues = issues
    )
    return(returnVal)
  }

  moduleFolder <- getModuleFolder(module, version)
  if (!dir.exists(moduleFolder)) {
    if (!silent) {
      warn("Module ", module, ", Version: ", version, " not found at: ", moduleFolder, ". This means the module was never installed.")
    }
    return(
      verifyModuleInstallationReturnValue(
        moduleFolder = moduleFolder,
        moduleInstalled = FALSE
        )
    )
  }

  if (!silent) {
    message("Verifying module: ", module, ", (", version, ") at ", moduleFolder, "...", appendLF = F)
  }
  moduleStatusFileName <- "moduleStatus.txt"
  renvLockFileName <- "renv.lock"

  # If the lock file doesn't exist, we're not sure if we're dealing with a module.
  if (!file.exists(file.path(moduleFolder, renvLockFileName))) {
    if (!silent) {
      message("ERROR - renv.lock file missing.")
    }
    return(
      verifyModuleInstallationReturnValue(
        moduleFolder = moduleFolder,
        moduleInstalled = FALSE
      )
    )
  }

  # Check to see if we've already performed the verification by looking at the
  # moduleStatus.txt file to see if the md5 in that file matches the one
  # created by hashing the renv.lock file
  lockfileContents <- ParallelLogger::loadSettingsFromJson(
    fileName = file.path(moduleFolder, renvLockFileName)
  )
  lockfileHash <- digest::digest(
    object = lockfileContents,
    algo = "md5"
  )
  if (!forceVerification && file.exists(file.path(moduleFolder, moduleStatusFileName))) {
    lockfileHashFromModuleStatusFile <- SqlRender::readSql(
      sourceFile = file.path(moduleFolder, moduleStatusFileName)
    )

    # If the values match, the module is installed correctly
    # return and exit
    if (lockfileHashFromModuleStatusFile == lockfileHash) {
      if (!silent) {
        message("MODULE READY!")
      }
      return(
        verifyModuleInstallationReturnValue(
          moduleFolder = moduleFolder,
          moduleInstalled = TRUE
          )
      )
    }
  }


  # Now perform the consistency check to verify that the renv::restore()
  # process executed successfully. We must do this in the module's context
  Strategus:::withModuleRenv(
    code = {
      # Start by turning off verbose output to hide renv output
      verboseOption <- getOption("renv.verbose")
      options(renv.verbose = FALSE)
      on.exit(options(renv.verbose = verboseOption))

      # Get the renv project status and then identify the packages used
      # in the project to determine if there were issues when restoring
      # the project from the renv.lock file.
      projectStatus <- renv::status()

      # Get the packages in the project - adapted from
      # https://github.com/rstudio/renv/blob/v1.0.3/R/status.R
      project <- renv:::renv_project_resolve()
      libpaths <- renv:::renv_libpaths_resolve()
      dependencies <- renv:::renv_snapshot_dependencies(project, dev = FALSE)
      packages <- sort(union(dependencies, "renv"))
      paths <- renv:::renv_package_dependencies(packages, libpaths = libpaths, project = project)
      packages <- as.character(names(paths))
      # remove ignored packages
      ignored <- c(
        renv:::renv_project_ignored_packages(project),
        renv:::renv_packages_base()
      )
      packages <- setdiff(packages, ignored)
      projectStatus$packages <- packages
      saveRDS(projectStatus, file="projectStatus.rds")
    },
    moduleFolder = moduleFolder
  )

  # The module's project status is written to the
  # file system. Now we can get the module status and use the information
  # to determine the restoration status
  projectStatus <- readRDS(file.path(moduleFolder, "projectStatus.rds"))

  library <- names(projectStatus$library$Packages)
  lockfile <- names(projectStatus$lockfile$Packages)

  packages <- sort(unique(c(library, lockfile, projectStatus$packages)))

  packageStatus <- data.frame(
    package = packages,
    installed = packages %in% library,
    recorded = packages %in% lockfile,
    used = packages %in% packages
  )

  # If all of the used & recorded packages are installed, then
  # return TRUE for the module installed status. If not, return
  # FALSE and set an attribute of the list that contains the issues
  # discovered
  ok <- packageStatus$installed & (packageStatus$used == packageStatus$recorded)
  issues <- packageStatus[!ok, , drop = FALSE]
  missing <- !issues$installed
  issues$installed <- ifelse(issues$installed, "y", "n")
  issues$recorded <- ifelse(issues$recorded, "y", "n")
  issues$used <- ifelse(issues$used, "y", if (any(missing)) "?" else "n")
  issues <- issues[issues$installed == "n" & issues$recorded == "y" & issues$used == "y", ]

  moduleInstalled <- nrow(issues) == 0

  if (isTRUE(moduleInstalled)) {
    if (!silent) {
      message("MODULE READY!")
    }
    # Write the contents of the md5 hash of the module's
    # renv.lock file to the file system to note that the
    # module's install status was successful and verified
    SqlRender::writeSql(
      sql = lockfileHash,
      targetFile = file.path(moduleFolder, "moduleStatus.txt")
    )
  } else {
    if (!silent) {
      message("MODULE HAS ISSUES!")
    }
  }

  return(
    verifyModuleInstallationReturnValue(
      moduleFolder = moduleFolder,
      moduleInstalled = moduleInstalled,
      issues = issues
    )
  )
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
      distinct(module, version, .keep_all = TRUE)
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
  assertModulesFolderSetting(x = Sys.getenv("INSTANTIATED_MODULES_FOLDER"))
  moduleFolder <- file.path(Sys.getenv("INSTANTIATED_MODULES_FOLDER"), sprintf("%s_%s", module, version))
  invisible(moduleFolder)
}

ensureModuleInstantiated <- function(module, version, remoteRepo, remoteUsername) {
  assertModulesFolderSetting(x = Sys.getenv("INSTANTIATED_MODULES_FOLDER"))
  instantiatedModulesFolder <- Sys.getenv("INSTANTIATED_MODULES_FOLDER")
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
  moduleFile <- file.path(moduleFolder, sprintf("%s_%s.zip", module, version))
  if (module == "TestModule1") {
    # For unit testing purposes only: get module from inst/testdata folder
    file.copy(
      from = system.file(
        file.path("testdata", basename(moduleFile)),
        package = utils::packageName()
      ),
      to = moduleFolder
    )
  } else {
    moduleUrl <- sprintf("https://%s/%s/%s/archive/refs/tags/v%s.zip", remoteRepo, remoteUsername, module, version)
    utils::download.file(url = moduleUrl, destfile = moduleFile)
  }
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

  withModuleRenv(
    code = {
      ParallelLogger::addDefaultFileLogger(
        fileName = file.path(moduleFolder, "moduleInitLog.txt")
      )
      ParallelLogger::addDefaultErrorReportLogger(
        fileName = file.path(moduleFolder, "moduleInitErrorReport.txt")
      )
      renv::restore(prompt = FALSE)
    },
    moduleFolder = moduleFolder,
    injectVars = list(moduleFolder = moduleFolder)
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
    dplyr::mutate(fileExists = file.exists(file.path(moduleFolder, value))) %>%
    dplyr::rename(fileName = value) %>%
    dplyr::select("fileName", "fileExists") %>%
    dplyr::filter(fileExists == FALSE)

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
