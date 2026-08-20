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
      cohortDefinitionSet <- private$.getCohortDefinitionSet(spec)
      private$.validateReferencedCohorts(
        pheValuatorAnalysisList = spec$pheValuatorAnalysisList,
        cohortDefinitionSet = cohortDefinitionSet
      )

      # Build the output folder for PheValuator
      outputFolder <- file.path(workFolder, "PheValuatorOutput")
      if (!dir.exists(outputFolder)) {
        dir.create(outputFolder, recursive = TRUE)
      }

      # Loop over each analysis in pheValuatorAnalysisList and execute
      nAnalyses <- length(spec$pheValuatorAnalysisList)
      for (i in seq_along(spec$pheValuatorAnalysisList)) {
        analysisSpec <- spec$pheValuatorAnalysisList[[i]]
        private$.message(sprintf(
          "Running PheValuator analysis %d of %d: '%s'",
          i, nAnalyses, analysisSpec$phenotype
        ))
        private$.executeAnalysis(
          analysisSpec = analysisSpec,
          cohortDefinitionSet = cohortDefinitionSet,
          analysisName = spec$analysisName,
          connectionDetails = connectionDetails,
          executionSettings = executionSettings,
          jobContext = jobContext,
          outputFolder = outputFolder,
          resultsFolder = resultsFolder
        )
        private$.message(sprintf(
          "Completed PheValuator analysis %d of %d: '%s'",
          i, nAnalyses, analysisSpec$phenotype
        ))
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
    #' @param analysisName A short name for the analysis (default: \code{"Main"}).
    #' @param pheValuatorAnalysisList A list of analysis specification objects.
    #'   Each element is a list with two named fields:
    #'   \describe{
    #'     \item{\code{phenotype}}{A short file-system-safe name for the phenotype
    #'       (e.g. \code{"bladderCancer"}).}
    #'     \item{\code{cohortsToEvaluate}}{A list of evaluation parameters:
    #'       \describe{
    #'         \item{\code{phenotypeCohortId}}{Cohort ID(s) of the phenotype to evaluate.}
    #'         \item{\code{washoutPeriod}}{Minimum prior observation days (should match cohort definition).}
    #'         \item{\code{xSpecCohortId}}{Cohort ID for the extremely-specific (xSpec) cohort.}
    #'         \item{\code{daysFromxSpec}}{Days from xSpec cohort start to index visit.}
    #'         \item{\code{xSensCohortId}}{Cohort ID for the extremely-sensitive (xSens) cohort.}
    #'         \item{\code{prevalenceCohortId}}{Cohort ID used to estimate prevalence.}
    #'         \item{\code{excludedCovariateConceptIds}}{Integer vector of concept IDs to exclude from covariates.}
    #'         \item{\code{covariateSettingsType}}{One of \code{"chronic"} or \code{"acute"} (default \code{"chronic"}).}
    #'       }
    #'     }
    #'   }
    createModuleSpecifications = function(analysisName = "Main",
                                          pheValuatorAnalysisList) {
      checkmate::assertList(pheValuatorAnalysisList, min.len = 1)

      analysis <- list(
        analysisName = analysisName,
        pheValuatorAnalysisList = pheValuatorAnalysisList
      )

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
      checkmate::assertList(moduleSpecifications$settings$pheValuatorAnalysisList, min.len = 1)
      for (a in moduleSpecifications$settings$pheValuatorAnalysisList) {
        checkmate::assertString(a$phenotype, min.chars = 1)
        checkmate::assertList(a$cohortsToEvaluate)
      }
    }
  ),
  private = list(
    .getResultsDataModelSpecificationFileLocation = function() {
      return(system.file(
        file.path("csv", "pheValuatorRdms.csv"),
        package = "Strategus"
      ))
    },

    .getCohortDefinitionSet = function(spec) {
      if (length(private$jobContext$sharedResources) > 0) {
        return(super$.createCohortDefinitionSetFromJobContext())
      }
      stop("PheValuator requires cohort definitions provided via Strategus Shared Resources.")
    },

    .getReferencedCohorts = function(pheValuatorAnalysisList) {
      referencedCohorts <- lapply(seq_along(pheValuatorAnalysisList), function(i) {
        cohortsToEvaluate <- pheValuatorAnalysisList[[i]]$cohortsToEvaluate
        cohortFields <- c("phenotypeCohortId", "xSpecCohortId", "xSensCohortId", "prevalenceCohortId")
        do.call(rbind, lapply(cohortFields, function(fieldName) {
          cohortIds <- cohortsToEvaluate[[fieldName]]
          if (is.null(cohortIds) || length(cohortIds) == 0) {
            return(NULL)
          }
          data.frame(
            analysisIndex = i,
            fieldName = fieldName,
            cohortId = as.integer(cohortIds),
            stringsAsFactors = FALSE
          )
        }))
      })
      referencedCohorts <- dplyr::bind_rows(referencedCohorts)
      if (nrow(referencedCohorts) == 0) {
        return(data.frame(
          analysisIndex = integer(),
          fieldName = character(),
          cohortId = integer(),
          stringsAsFactors = FALSE
        ))
      }
      referencedCohorts <- referencedCohorts[!is.na(referencedCohorts$cohortId), , drop = FALSE]
      return(referencedCohorts)
    },

    .validateReferencedCohorts = function(pheValuatorAnalysisList, cohortDefinitionSet) {
      referencedCohorts <- private$.getReferencedCohorts(pheValuatorAnalysisList)
      if (nrow(referencedCohorts) == 0) {
        return(invisible(TRUE))
      }

      definedCohortIds <- as.integer(cohortDefinitionSet$cohortId)
      missingCohorts <- referencedCohorts[!(referencedCohorts$cohortId %in% definedCohortIds), , drop = FALSE]
      if (nrow(missingCohorts) > 0) {
        missingCohorts <- unique(missingCohorts)
        missingDescriptions <- apply(missingCohorts, 1, function(row) {
          paste0("analysis ", row[["analysisIndex"]], " field ", row[["fieldName"]], " cohortId ", row[["cohortId"]])
        })
        stop(
          paste0(
            "PheValuator cohort definitions are missing from Strategus Shared Resources/cohortDefinitionSet: ",
            paste(missingDescriptions, collapse = "; ")
          )
        )
      }
      invisible(TRUE)
    },

    .subsetCohortDefinitionSet = function(cohortDefinitionSet, pheValuatorAnalysisList) {
      referencedCohorts <- private$.getReferencedCohorts(pheValuatorAnalysisList)
      referencedCohortIds <- unique(referencedCohorts$cohortId)
      cohortDefinitionSet[as.integer(cohortDefinitionSet$cohortId) %in% referencedCohortIds, , drop = FALSE]
    },

    .formatCohortDefinitionSetForPheValuator = function(cohortDefinitionSet) {
      requiredColumns <- c("cohortId", "cohortName", "json", "sql")
      missingColumns <- setdiff(requiredColumns, colnames(cohortDefinitionSet))
      if (length(missingColumns) > 0) {
        stop("cohortDefinitionSet is missing required columns: ", paste(missingColumns, collapse = ", "))
      }
      extraColumns <- setdiff(colnames(cohortDefinitionSet), requiredColumns)
      cohortDefinitionSet[, c(requiredColumns, extraColumns), drop = FALSE]
    },

    # Execute a single analysis spec entry
    .executeAnalysis = function(analysisSpec,
                                cohortDefinitionSet,
                                analysisName,
                                connectionDetails,
                                executionSettings,
                                jobContext,
                                outputFolder,
                                resultsFolder) {
      cts <- analysisSpec$cohortsToEvaluate
      phenotype <- analysisSpec$phenotype

      if (length(cts$phenotypeCohortId) != length(cts$washoutPeriod)) {
        stop("Length of phenotypeCohortId and washoutPeriod must be the same.")
      }

      # Evaluate xSens and xSpec by default 
      cts$phenotypeCohortId <-
        c(cts$phenotypeCohortId,
          cts$xSpecCohortId,
          cts$xSensCohortId)
      
      cts$washoutPeriod <-
        c(cts$washoutPeriod, 0, 0)


      # Build one pheValuatorAnalysis per phenotypeCohortId
      phenotypeCohortIds <- as.integer(cts$phenotypeCohortId)
      washoutPeriods <- rep_len(as.integer(cts$washoutPeriod), length(phenotypeCohortIds))

      pheValuatorAnalysisList <- mapply(
        function(cohortId, washout, idx) {
          createEvaluationCohortArgs <- PheValuator::createCreateEvaluationCohortArgs(
            xSpecCohortId      = as.integer(cts$xSpecCohortId),
            daysFromxSpec      = as.integer(cts$daysFromxSpec %||% 0),
            xSensCohortId      = as.integer(cts$xSensCohortId),
            prevalenceCohortId = as.integer(cts$prevalenceCohortId),
            covariateSettings  = cts$covariateSettings,
            lowerAgeLimit = as.integer(cts$lowerAgeLimit),
            upperAgeLimit = as.integer(cts$upperAgeLimit)
          )
          testPhenotypeAlgorithmArgs <- PheValuator::createTestPhenotypeAlgorithmArgs(
            phenotypeCohortId = cohortId,
            washoutPeriod     = washout,
            cutPoints         = c("EV")
          )
          PheValuator::createPheValuatorAnalysis(
            analysisId                 = idx,
            description                = paste0(phenotype, "_cohort", cohortId),
            createEvaluationCohortArgs = createEvaluationCohortArgs,
            testPhenotypeAlgorithmArgs = testPhenotypeAlgorithmArgs
          )
        },
        phenotypeCohortIds,
        washoutPeriods,
        seq_along(phenotypeCohortIds),
        SIMPLIFY = FALSE
      )

      # Per-phenotype output sub-folder
      phenotypeOutputFolder <- file.path(outputFolder, phenotype)
      if (!dir.exists(phenotypeOutputFolder)) {
        dir.create(phenotypeOutputFolder, recursive = TRUE)
      }

      PheValuator::runPheValuatorAnalyses(
        phenotype          = phenotype,
        cohortDefinitionSet = private$.formatCohortDefinitionSetForPheValuator(
          private$.subsetCohortDefinitionSet(
            cohortDefinitionSet = cohortDefinitionSet,
            pheValuatorAnalysisList = list(analysisSpec)
          )
        ),
        analysisName       = analysisName,
        connectionDetails  = connectionDetails,
        tempEmulationSchema = executionSettings$tempEmulationSchema,
        cdmDatabaseSchema  = executionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = executionSettings$workDatabaseSchema,
        cohortTable        = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        workDatabaseSchema = executionSettings$workDatabaseSchema,
        databaseId         = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        outputFolder       = phenotypeOutputFolder,
        pheValuatorAnalysisList = pheValuatorAnalysisList
      )

      # Copy CSV results to results folder, appending to existing files
      exportFolder <- file.path(phenotypeOutputFolder, "exportFolder")
      if (dir.exists(exportFolder)) {
        csvFiles <- list.files(exportFolder, pattern = "\\.csv$", full.names = TRUE)
        for (csvFile in csvFiles) {
          targetFileName <- basename(csvFile)
          if (!startsWith(targetFileName, self$tablePrefix)) {
            targetFileName <- paste0(self$tablePrefix, targetFileName)
          }
          
          targetFilePath <- file.path(resultsFolder, targetFileName)
          newData <- CohortGenerator::readCsv(
            file = csvFile,
            warnOnCaseMismatch = FALSE
          )
          
          # If the file already exists, append to it; otherwise create it
          if (file.exists(targetFilePath)) {
            existingData <- CohortGenerator::readCsv(
              file = targetFilePath,
              warnOnCaseMismatch = FALSE
            )
            combinedData <- dplyr::bind_rows(existingData, newData)
          } else {
            combinedData <- newData
          }
          
          CohortGenerator::writeCsv(
            x = combinedData,
            file = targetFilePath,
            warnOnCaseMismatch = FALSE,
            warnOnFileNameCaseMismatch = FALSE,
            warnOnUploadRuleViolations = FALSE
          )
        }
      }
    }
  )
)
