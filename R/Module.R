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


#' Load a StrategusModule s3 class
#' @export
#' @description
#'
#' Returns a list of module info for a given module
#'
loadModule <- function(moduleName = null, version = NULL, folder = NULL, instantiate = FALSE) {
  available <- listAvailableModules()
  module <- list(

  )
  class(module) <- "StrategusModule"
  return(module)
}


#' Module exec
#' @description
#'
#' Execute ariibtrary code inside a strategus module.
#' This is mainly intended for moudle developers that wish to debug problems.
#' However, it can be used, for example, to execute code with multiple different
#' versions of the same package inside an R session.
#'
#' In order to return data in to an R object you will need to write data to disk and read it back
#' Naturally, this will be much slower than if calling inside a session.
#'
#' @param job                                  Optionally run this task inside an R studio job. This is useful if you
#'                                             want to spawn lots of independent processes. No handling of what happens
#'                                             when this occurs is handled with this function call
#'
#' @param useLocalStrategusLibrary             On some systems finding the base Strategus package can fail.
#'                                             By setting this to true the execution of the code will first load the
#'                                             calling Strategus library. This is only needed if you are calling other
#'                                             strategus functions
#'
moduleExec <- function(module, code, substituteVars = list(), useLocalStrategusLibrary = FALSE, job = job) {
  checkmate::expect_class(module, "StrategusModule")

  tempFile <- withModuleRenv(code,
                             module$moduleFolder,
                             injectVars = list(),
                             tempScriptFile = tempfile(fileext = ".R"),
                             useLocalStrategusLibrary = useLocalStrategusLibrary,
                             job = job,
                             processName = paste(moduleFolder, "_renv_run"))

  unlink(tempFile)
}