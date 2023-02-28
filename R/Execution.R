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

# Note: Using S3 for consistency with settings objects in PLP, CohortMethod, and
# FeatureExtraction. If we want to use S4 or R6 we need to first adapt those
# packages. This will be difficult, since these settings objects are used throughout
# these packages, and are for example used in do.call() calls. We should also
# carefully consider serialization and deserialization to JSON, which currently
# uses custom functionality in ParallelLogger to maintain object attributes.

#' Execute analysis specifications.
#'
#' @template AnalysisSpecifications
#' @param executionSettings       An object of type `ExecutionSettings` as created
#'                                by [createCdmExecutionSettings()] or [createResultsExecutionSettings()].
#' @param executionScriptFolder   Optional: the path to use for storing the execution script.
#'                                when NULL, this function will use a temporary
#'                                file location to create the script to execute.
#'
#' @template keyringName
#'
#' @param restart                 Restart run? Requires `executionScriptFolder` to be specified, and be
#'                                the same as the `executionScriptFolder` used in the run to restart.
#'
#'
#' @return
#' Does not return anything. Is called for the side-effect of executing the specified
#' analyses.
#'
#' @export
execute <- function(analysisSpecifications,
                    executionSettings,
                    executionScriptFolder = NULL,
                    keyringName = NULL,
                    restart = FALSE) {
  errorMessages <- checkmate::makeAssertCollection()
  keyringList <- keyring::keyring_list()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(executionSettings, "ExecutionSettings", add = errorMessages)
  checkmate::assertChoice(x = keyringName, choices = keyringList$keyring, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  modules <- ensureAllModulesInstantiated(analysisSpecifications)

  if (is.null(executionScriptFolder)) {
    executionScriptFolder <- tempfile("strategusTempSettings")
    dir.create(executionScriptFolder)
    on.exit(unlink(executionScriptFolder, recursive = TRUE))
  } else if (!restart) {
    if (dir.exists(executionScriptFolder)) {
      unlink(executionScriptFolder, recursive = TRUE)
    }
    dir.create(executionScriptFolder, recursive = TRUE)
  }

  if (is(executionSettings, "CdmExecutionSettings")) {
    executionSettings$databaseId <- createDatabaseMetaData(
      executionSettings = executionSettings,
      keyringName = keyringName
    )
  }
  dependencies <- extractDependencies(modules)

  fileName <- generateTargetsScript(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    dependencies = dependencies,
    executionScriptFolder = executionScriptFolder,
    restart = restart,
    keyringName = keyringName
  )
  # targets::tar_manifest(script = fileName)
  # targets::tar_glimpse(script = fileName)
  targets::tar_make(script = fileName, store = file.path(executionScriptFolder, "_targets"))
}

generateTargetsScript <- function(analysisSpecifications, executionSettings, dependencies, executionScriptFolder, keyringName, restart) {
  fileName <- file.path(executionScriptFolder, "script.R")
  if (restart) {
    return(fileName)
  }
  # Store settings objects in the temp folder so they are available in targets
  analysisSpecificationsFileName <- gsub("\\\\", "/", file.path(executionScriptFolder, "analysisSpecifications.rds"))
  saveRDS(analysisSpecifications, analysisSpecificationsFileName)
  executionSettingsFileName <- gsub("\\\\", "/", file.path(executionScriptFolder, "executionSettings.rds"))
  saveRDS(executionSettings, executionSettingsFileName)
  keyringSettingsFileName <- gsub("\\\\", "/", file.path(executionScriptFolder, "keyringSettings.rds"))
  saveRDS(list(keyringName = keyringName), keyringSettingsFileName)


  # Dynamically generate targets script based on analysis specifications
  lines <- c(
    "library(targets)",
    "tar_option_set(packages = c('Strategus', 'keyring'))",
    "list(",
    "  tar_target(",
    "    analysisSpecifications,",
    sprintf("    readRDS('%s')", analysisSpecificationsFileName),
    "  ),",
    "  tar_target(",
    "    executionSettings,",
    sprintf("    readRDS('%s')", executionSettingsFileName),
    "  ),",
    "  tar_target(",
    "    keyringSettings,",
    sprintf("    readRDS('%s')", keyringSettingsFileName),
    "  ),"
  )
  # Generate target names by module type
  moduleToTargetNames <- list()
  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleSpecification <- analysisSpecifications$moduleSpecifications[[i]]
    targetName <- sprintf("%s_%d", moduleSpecification$module, i)
    moduleToTargetNames[[length(moduleToTargetNames) + 1]] <- tibble(
      module = moduleSpecification$module,
      targetName = targetName
    )
  }
  moduleToTargetNames <- bind_rows(moduleToTargetNames)

  execResultsUpload <- all(c(is(executionSettings, "CdmExecutionSettings"),
                             !is.null(executionSettings$resultsConnectionDetailsReference),
                             !is.null(executionSettings$resultsDatabaseSchema)))

  # Generate targets code, inserting dependencies
  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleSpecification <- analysisSpecifications$moduleSpecifications[[i]]
    targetName <- sprintf("%s_%d", moduleSpecification$module, i)
    dependencyModules <- dependencies %>%
      filter(.data$module == moduleSpecification$module) %>%
      pull(.data$dependsOn)
    dependencyTargetNames <- moduleToTargetNames %>%
      filter(.data$module %in% dependencyModules) %>%
      pull(.data$targetName)

    command <- sprintf(
      "Strategus:::runModule(analysisSpecifications, keyringSettings, %d, executionSettings%s)",
      i,
      ifelse(length(dependencyTargetNames) == 0, "", sprintf(", %s", paste(dependencyTargetNames, collapse = ", ")))
    )


    lines <- c(
      lines,
      "  tar_target(",
      sprintf("    %s,", targetName),
      sprintf("    %s", command),
      ifelse(execResultsUpload || i == length(analysisSpecifications$moduleSpecifications), "  )", "  ),")
    )
  }

  # Automatic results upload if settings are specified
  if (execResultsUpload) {
    # Generate targets code for results upload, inserting dependencies on parent module
    for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
      moduleSpecification <- analysisSpecifications$moduleSpecifications[[i]]
      # The only dependency for a module's results is the module execution itself
      dependencyTargetNames <- sprintf("%s_%d", moduleSpecification$module, i)
      targetName <- sprintf("%s_%d_results_upload", moduleSpecification$module, i)

      command <- sprintf(
        "Strategus:::runResultsUpload(analysisSpecifications, keyringSettings, %d, executionSettings%s)",
        i,
        sprintf(", %s", paste(dependencyTargetNames, collapse = ", "))
      )

      lines <- c(
        lines,
        "tar_target(",
        sprintf("    %s,", targetName),
        sprintf("    %s", command),
        ifelse(i == length(analysisSpecifications$moduleSpecifications), "  )", "  ),")
      )
    }
  }
  lines <- c(lines, ")")

  sink(fileName)
  cat(paste(lines, collapse = "\n"))
  sink()
  return(fileName)
}

