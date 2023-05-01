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

.handleInjectVar <- function (x) {
  hVar <- function(x) {
    if (is.character(x)) {
      return(sprintf('"%s"', x))
    } else {
      return(sprintf('%s', x))
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
#' Designed to allow more human readable code that is executed inside a module as well as simple variable substituion
#' for injecting constants (e.g. simple parameters or file paths used inside and outside of modules)
#'
#' This pattern also allows dependency injection which could be used if you don't want to use and renv and (instead)
#' would like to use docker images or just execution in the base environment for testing/debugging
#'
#' @param code              code block to execute
#' @param moduleFolder      Instantiated Strategus module folder
#' @param injectVars         list of var names list(name=value) to replace (e.g. replace list(foo = "some string") will
#'                          find the pattern foo and replace it with the string some string - be careful!
#' @param tempScriptFile    tempFile to write script to (ret
#' @param job               run as rstudio job
#' @param processName       String name for process
#' @returns NULL invisibly
withModuleRenv <- function(code,
                           moduleFolder,
                           injectVars = list(),
                           tempScriptFile = tempfile(fileext = ".R"),
                           useLocalStrategusLibrary = TRUE,
                           job = FALSE,
                           processName = paste(moduleFolder, "_renv_run")) {
  # convert human readable code to a string for writing
  script <- as.character(substitute(code))[-1]
  # Insert variables
  for (name in names(injectVars)) {
    rep <- .handleInjectVar(injectVars[[name]])
    script <- gsub(name, rep, script)
  }

  # Enforce attachment of Strategus from calling process - note one inside the renv
  if (useLocalStrategusLibrary) {
    libPath <- file.path(find.package("Strategus"), "../")
    script <- c(sprintf("library(Strategus, lib.loc = '%s')", libPath),
                script)
  }

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