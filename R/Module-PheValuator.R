# Copyright 2026 Observational Health Data Sciences and Informatics
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

#' @title Evaluate phenotype algorithms with the \href{https://ohdsi.github.io/PheValuator/}{HADES PheValuator Package}
#' @export
#' @description
#' Evaluates phenotype algorithms using the PheValuator method against the OMOP Common Data Model.
PheValuatorModule <- R6::R6Class(
  classname = "PheValuatorModule",
  inherit = StrategusModule,

  ## Public ----
  public = list(
    ### Fields ----
    #' @field tablePrefix The table prefix to append to the results tables
    tablePrefix = "pv_",

    ### Methods ----
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },

    #' @description Execute PheValuator
    #'
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      spec <- jobContext$settings

      # Build the output folder for PheValuator
      outputFolder <- file.path(workFolder, "PheValuatorOutput")
      if (!dir.exists(outputFolder)) {
        dir.create(outputFolder, recursive = TRUE)
      }

      # Extract settings
      phenotype <- spec$phenotype
      analysisName <- spec$analysisName %||% "Main"
      cohortDefinitionSet <- NULL
      if (!is.null(spec$cohortDefinitionSet)) {
        cohortDefinitionSet <- super$.listToDataFrame(spec$cohortDefinitionSet)
      } else {
        cohortDefinitionSet <- data.frame()
      }

      pheValuatorAnalysisList <- spec$pheValuatorAnalysisList

      # Run PheValuator analyses
      referenceTable <- PheValuator::runPheValuatorAnalyses(
        phenotype = phenotype,
        cohortDefinitionSet = cohortDefinitionSet,
        analysisName = analysisName,
        connectionDetails = connectionDetails,
        tempEmulationSchema = executionSettings$tempEmulationSchema,
        cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = executionSettings$workDatabaseSchema,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        workDatabaseSchema = executionSettings$workDatabaseSchema,
        databaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        outputFolder = outputFolder,
        pheValuatorAnalysisList = pheValuatorAnalysisList
      )

      # Export results
      if (!dir.exists(resultsFolder)) {
        dir.create(resultsFolder, recursive = TRUE)
      }

      # Copy the CSV files produced by PheValuator to the results folder
      exportFolder <- file.path(outputFolder, "exportFolder")
      if (dir.exists(exportFolder)) {
        csvFiles <- list.files(exportFolder, pattern = "\\.csv$", full.names = TRUE)
        for (csvFile in csvFiles) {
          targetFileName <- basename(csvFile)
          # Ensure table prefix is applied
          if (!startsWith(targetFileName, self$tablePrefix)) {
            targetFileName <- paste0(self$tablePrefix, targetFileName)
          }
          file.copy(csvFile, file.path(resultsFolder, targetFileName), overwrite = TRUE)
        }
      }

      # Export the resultsDataModelSpecification.csv
      resultsDataModelSpecification <- self$getResultsDataModelSpecification()
      CohortGenerator::writeCsv(
        x = resultsDataModelSpecification,
        file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
        warnOnCaseMismatch = FALSE,
        warnOnFileNameCaseMismatch = FALSE,
        warnOnUploadRuleViolations = FALSE
      )

      private$.message(paste("Results available at:", resultsFolder))
    },

    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      if (resultsConnectionDetails$dbms == "sqlite" & resultsDatabaseSchema != "main") {
        stop("Invalid schema for sqlite, use databaseSchema = 'main'")
      }

      connection <- DatabaseConnector::connect(resultsConnectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))

      # Create the results model
      sql <- ResultModelManager::generateSqlSchema(schemaDefinition = self$getResultsDataModelSpecification())
      sql <- SqlRender::render(sql = sql, warnOnMissingParameters = TRUE, database_schema = resultsDatabaseSchema)
      sql <- SqlRender::translate(sql = sql, targetDialect = resultsConnectionDetails$dbms)
      DatabaseConnector::executeSql(connection, sql)
    },

    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = private$.getResultsDataModelSpecificationFileLocation(),
        warnOnCaseMismatch = FALSE
      )
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, self$tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },

    #' @description Upload the results for PheValuator
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)
      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      resultsModelSpec <- self$getResultsDataModelSpecification()

      ResultModelManager::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        resultsFolder = resultsFolder,
        purgeSiteDataBeforeUploading = FALSE,
        specifications = resultsModelSpec
      )
    },

    #' @description Creates the PheValuator Module Specifications
    #'
    #' @param phenotype The name of the phenotype being evaluated
    #' @param analysisName The name of the analysis (default: "Main")
    #' @param cohortDefinitionSet A data.frame of cohort definitions with columns
    #'   cohortId, cohortName, json, sql. Should include all cohort definitions
    #'   needed to replicate the PheValuator analysis. If NULL, an empty data.frame
    #'   will be used.
    #' @param pheValuatorAnalysisList A list of PheValuator analysis objects
    #'   created using \code{PheValuator::createPheValuatorAnalysis()}
    createModuleSpecifications = function(phenotype,
                                          analysisName = "Main",
                                          cohortDefinitionSet = NULL,
                                          pheValuatorAnalysisList) {
      analysis <- list()
      analysis$phenotype <- phenotype
      analysis$analysisName <- analysisName
      if (!is.null(cohortDefinitionSet) && nrow(cohortDefinitionSet) > 0) {
        analysis$cohortDefinitionSet <- super$.dataFrameToList(cohortDefinitionSet)
      } else {
        analysis$cohortDefinitionSet <- NULL
      }
      analysis$pheValuatorAnalysisList <- pheValuatorAnalysisList

      specification <- super$createModuleSpecifications(analysis)
      return(specification)
    },

    #' @description Validate the module specifications
    #'
    #' @param moduleSpecifications The PheValuator module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
      # Validate required fields
      checkmate::assertCharacter(moduleSpecifications$settings$phenotype, min.chars = 1)
      checkmate::assertList(moduleSpecifications$settings$pheValuatorAnalysisList, min.len = 1)
    }
  ),
  private = list(
    .getResultsDataModelSpecificationFileLocation = function() {
      return(system.file(
        file.path("csv", "pheValuatorRdms.csv"),
        package = "Strategus"
      ))
    }
  )
)
