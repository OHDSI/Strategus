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


listAvailableModules <- function(systemModulePath = Sys.getenv("STRATEGUS_INSTALLED_MODULES"),
                                 localModulePath = file.path(gwetwd(), "strategus_modules"),
                                 refreshCache = FALSE) {

  if (systemModulePath == "") {
    warning("No system module path set, set STRATEGUS_INSTALLED_MODULES in your .renviron to enable global modules")
  }

  data.frame(module = c(),
             modulePath = c(),
             version = c())
}

#' Install a module into the
#'
#'
installLocalModule <- function(pathToModule,
                               instantiate = FALSE,
                               systemModulePath = Sys.getenv("STRATEGUS_INSTALLED_MODULES"),
                               localModulePath = file.path(gwetwd(), "strategus_modules"),
                               installPath = systemModulePath[-1]) {
  # Validate module files

  # lock rds object

  # Write to install path

  # save rds

  if (instantiate) {

  }
}

installGithubModule <- function(repoPath, ref = NULL, ...) {
  availableModules <- listAvailableModules()
  # Download tarball
}