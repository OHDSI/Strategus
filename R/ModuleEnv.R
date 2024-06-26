# Copyright 2024 Observational Health Data Sciences and Informatics
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

.handleInjectVar <- function(x) {
  hVar <- function(x) {
    if (is.character(x)) {
      return(sprintf('"%s"', x))
    } else {
      return(sprintf("%s", x))
    }
  }

  if (length(x) == 1) {
    return(hVar(x))
  } else if (is.vector(x)) {
    innerVars <- hVar(x)
    return(paste0("c(", paste(innerVars, collapse = ", "), ")"))
  } else {
    stop("cannot handle complex data structures in variable injection")
  }
}

#' Load module execution space inside and renv
#' inspired by targets::tar_script but allowing custom variable execution
#'
#' Designed to allow more human readable code that is executed inside a module as well as simple variable substitution
#' for injecting constants (e.g. simple parameters or file paths used inside and outside of modules)
#'
#' This pattern also allows dependency injection which could be used if you don't want to use and renv and (instead)
#' would like to use docker images or just execution in the base environment for testing/debugging
#'
#' @param code              code block to execute
#' @param moduleFolder      Instantiated Strategus module folder
#' @param injectVars        list of var names list(name=value) to replace (e.g. replace list(foo = "some string") will
#'                          find the pattern foo and replace it with the string some string - be careful!
#' @param tempScriptFile    tempFile to write script to
#' @param job               run as rstudio job
#' @param processName       String name for process
#' @returns NULL invisibly
withModuleRenv <- function(code,
                           moduleFolder,
                           injectVars = list(),
                           tempScriptFile = tempfile(fileext = ".R"),
                           job = FALSE,
                           processName = paste(moduleFolder, "_renv_run")) {
  # convert human readable code to a string for writing
  script <- as.character(substitute(code))[-1]
  # Insert variables
  for (name in names(injectVars)) {
    rep <- .handleInjectVar(injectVars[[name]])
    script <- gsub(name, rep, script)
  }

  # Attach renv options() from the calling environment to the renv::run context
  # renv options are prefixed with "renv." as described in
  # https://rstudio.github.io/renv/reference/config.html
  envOptions <- options()
  renvOptions <- envOptions[grepl("renv\\.", names(envOptions))]
  if (length(renvOptions) > 0) {
    for (i in 1:length(renvOptions)) {
      script <- c(.copyOptionForScript(
        optionName = names(renvOptions)[[i]],
        optionValue = renvOptions[[i]]
      ), script)
    }
  }

  # Turning off verbose output to hide renv output
  # unless the user has set this option to TRUE.
  if (!getOption(x = "renv.verbose", default = FALSE)) {
    options(renv.verbose = FALSE)
  }

  # Import the Strategus functions we need to use in the module scripts
  script <- c("retrieveConnectionDetails <- ", base::deparse(Strategus::retrieveConnectionDetails), script)
  script <- c("unlockKeyring <- ", base::deparse(Strategus::unlockKeyring), script)

  # Write file and execute script inside an renv
  fileConn <- file(tempScriptFile)
  writeLines(script, fileConn)
  close(fileConn)
  renv::run(
    script = tempScriptFile,
    job = job,
    name = processName,
    project = moduleFolder
  )
  return(invisible(NULL))
}

.getLocalLibraryScipt <- function(x) {
  libPath <- file.path(find.package(x), "../")
  sprintf("library(%s, lib.loc = '%s')", x, libPath)
}

.copyOptionForScript <- function(optionName, optionValue) {
  if (is.logical(optionValue) || is.numeric(optionValue)) {
    sprintf("options(%s = %s)", optionName, optionValue)
  } else if (is.character(optionValue) && length(optionValue) == 1) {
    sprintf("options(%s = '%s')", optionName, optionValue)
  } else if (is.character(optionValue) && length(optionValue) > 1) {
    sprintf("options(%s = c('%s'))", optionName, paste(optionValue, collapse = "','"))
  } else {
    paste0("# option = ", optionName, " - could not be passed to this file, likely because it is a function.")
  }
}

.formatAndNormalizeFilePathForScript <- function(filePath) {
  return(gsub("\\\\", "/", normalizePath(path = filePath, mustWork = F)))
}
