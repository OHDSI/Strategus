# Copyright 2025 Observational Health Data Sciences and Informatics
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

#' @title Evaluate phenotypes with the \href{https://github.com/darwin-eu/TreatmentPatterns/}{DARWIN TreatmentPatterns Package}
#' @export
#' @description
#' Characterization and description of patterns of events (cohorts). against the OMOP Common Data Model.
TreatmentPatternsModule <- R6::R6Class(
  classname = "TreatmentPatternsModule",
  inherit = StrategusModule,

  ## Public ----
  public = list(
    ### Fields ----
    #' @field tablePrefix The table prefix to append to the results tables
    tablePrefix = "tp_",

    ### Methods ----
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },

    #' @description Execute Treatment Patterns
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
      cohorts <- super$.listToDataFrame(spec$cohorts)

      outputEnv <- TreatmentPatterns::computePathways(
        cohorts = cohorts,
        cohortTableName = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        connectionDetails = connectionDetails,
        cdmSchema = executionSettings$cdmDatabaseSchema,
        resultSchema = executionSettings$workDatabaseSchema,
        tempEmulationSchema = executionSettings$tempEmulationSchema,
        startAnchor = spec$startAnchor,
        windowStart = spec$windowStart,
        endAnchor = spec$endAnchor,
        windowEnd = spec$windowEnd,
        minEraDuration = spec$minEraDuration,
        splitEventCohorts = spec$splitEventCohorts,
        splitTime = spec$splitTime,
        eraCollapseSize = spec$eraCollapseSize,
        combinationWindow = spec$combinationWindow,
        minPostCombinationDuration = spec$minPostCombinationDuration,
        filterTreatments = spec$filterTreatments,
        maxPathLength = spec$maxPathLength,
        overlapMethod = spec$overlapMethod,
        concatTargets = spec$concatTargets
      )

      TreatmentPatterns::export(
        andromeda = outputEnv,
        outputPath = resultsFolder,
        ageWindow = spec$ageWindow,
        minCellCount = executionSettings$minCellCount,
        censorType = spec$censorType,
        archiveName = NULL
      )

      on.exit(
        Andromeda::saveAndromeda(
          andromeda = outputEnv,
          fileName = file.path(workFolder, "outputEnv")
        )
      )

      # HACK: Append the database_id to all exported results
      csvFiles <- list.files(resultsFolder, pattern = "\\.csv$", full.names = TRUE)
      for (file in csvFiles) {
        if (tools::file_path_sans_ext(basename(file)) != "analyses") {
          data <- CohortGenerator::readCsv(
            file = file
          )
          data$databaseId <- jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId
          CohortGenerator::writeCsv(
            x = data,
            file = file,
            warnOnCaseMismatch = FALSE
          )
        }
      }

      # Rename all exported files to include the module prefix to the file name
      for (file in csvFiles) {
        newFileName <- file.path(resultsFolder, paste0(self$tablePrefix, basename(file)))
        file.rename(file, newFileName)
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

    #' @description Upload the results for TreatmentPatterns
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)
      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      exportFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      resultsModelSpec <- self$getResultsDataModelSpecification()

      ResultModelManager::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        resultsFolder = resultsFolder,
        purgeSiteDataBeforeUploading = FALSE,
        specifications = resultsModelSpec
      )
    },
    #' @description Creates the TreatmentPatternsnModule Specifications
    #'
    #' @param cohorts (`data.frame()`)\cr
    #' Data frame containing the following columns and data types:
    #' \describe{
    #'  \item{cohortId `numeric(1)`}{Cohort ID's of the cohorts to be used in the cohort table.}
    #'  \item{cohortName `character(1)`}{Cohort names of the cohorts to be used in the cohort table.}
    #'  \item{type `character(1)` \["target", "event', "exit"\]}{Cohort type, describing if the cohort is a target, event, or exit cohort}
    #' }
    #' @param minEraDuration (`integer(1)`: `0`)\cr
    #' Minimum time an event era should last to be included in analysis
    #' @param splitEventCohorts (`character(n)`: `""`)\cr
    #' Specify event cohort to split in acute (< X days) and therapy (>= X days)
    #' @param splitTime (`integer(1)`: `30`)\cr
    #' Specify number of days (X) at which each of the split event cohorts should
    #' be split in acute and therapy
    #' @param eraCollapseSize (`integer(1)`: `30`)\cr
    #' Window of time between which two eras of the same event cohort are collapsed
    #' into one era
    #' @param combinationWindow (`integer(1)`: `30`)\cr
    #' Window of time two event cohorts need to overlap to be considered a
    #' combination treatment
    #' @param minPostCombinationDuration (`integer(1)`: `30`)\cr
    #' Minimum time an event era before or after a generated combination treatment
    #' should last to be included in analysis
    #' @param filterTreatments (`character(1)`: `"First"` \["first", "Changes", "all"\])\cr
    #' Select first occurrence of (‘First’); changes between (‘Changes’); or all
    #' event cohorts (‘All’).
    #' @param maxPathLength (`integer(1)`: `5`)\cr
    #' Maximum number of steps included in treatment pathway
    #' @param ageWindow (`integer(n)`: `10`)\cr
    #' Number of years to bin age groups into. It may also be a vector of integers.
    #' I.e. `c(0, 18, 150)` which will results in age group `0-18` which includes
    #' subjects `< 19`. And age group `18-150` which includes subjects `> 18`.
    #' @param minCellCount (`integer(1)`: `5`)\cr
    #' Minimum count required per pathway. Censors data below `x` as `<x`. This
    #' minimum value will carry over to the sankey diagram and sunburst plot.
    #' @param censorType (`character(1)`)\cr
    #' \describe{
    #'   \item{`"minCellCount"`}{Censors pathways <`minCellCount` to `minCellCount`.}
    #'   \item{`"remove"`}{Censors pathways <`minCellCount` by removing them completely.}
    #'   \item{`"mean"`}{Censors pathways <`minCellCount` to the mean of all frequencies below `minCellCount`}
    #' }
    #' @param overlapMethod (`character(1)`: `"truncate"`) Method to decide how to deal
    #' with overlap that is not significant enough for combination. `"keep"` will
    #' keep the dates as is. `"truncate"` truncates the first occurring event to
    #' the start date of the next event.
    #' @param concatTargets (`logical(1)`: `TRUE`) Should multiple target cohorts for the same person be concatenated or not?
    #' @param startAnchor (`character(1)`: `"startDate"`) Start date anchor. One of: `"startDate"`, `"endDate"`
    #' @param windowStart (`numeric(1)`: `0`) Offset for `startAnchor` in days.
    #' @param endAnchor (`character(1)`: `"endDate"`) End date anchor. One of: `"startDate"`, `"endDate"`
    #' @param windowEnd (`numeric(1)`: `0`) Offset for `endAnchor` in days.
    #' @param indexDateOffset (`integer(1)`: `0`)\cr
    #' `DEPRECATED`
    #' Offset the index date of the `Target` cohort.
    #' @param includeTreatments (`character(1)`: `"startDate"`)\cr
    #' `DEPRECATED`
    #' \describe{
    #'  \item{`"startDate"`}{Include treatments after the target cohort start date and onwards.}
    #'  \item{`"endDate"`}{Include treatments before target cohort end date and before.}
    #' }
    createModuleSpecifications = function(cohorts,
                                          includeTreatments = NULL,
                                          indexDateOffset = NULL,
                                          minEraDuration = 0,
                                          splitEventCohorts = NULL,
                                          splitTime = NULL,
                                          eraCollapseSize = 30,
                                          combinationWindow = 30,
                                          minPostCombinationDuration = 30,
                                          filterTreatments = "First",
                                          maxPathLength = 5,
                                          ageWindow = 5,
                                          minCellCount = 1,
                                          censorType = "minCellCount",
                                          overlapMethod = "truncate",
                                          concatTargets = TRUE,
                                          startAnchor = "startDate",
                                          windowStart = 0,
                                          endAnchor = "endDate",
                                          windowEnd = 0) {
      if (!is.null(indexDateOffset)) {
        warning("`indexDateOffset` is deprecated in TreatmentPatterns 3.1.0, please use: `startAnchor`, `windowStart`, `endAnchor`, `windowEnd` instead.")
      }

      if (!is.null(includeTreatments)) {
        warning("`includeTreatments` is deprecated in TreatentPatterns 3.1.0, please use: `startAnchor`, `windowStart`, `endAnchor`, `windowEnd` instead.")
      }

      analysis <- list()
      for (name in names(formals(self$createModuleSpecifications))) {
        if (name == "cohorts") {
          analysis[[name]] <- super$.dataFrameToList(get(name))
        } else {
          analysis[[name]] <- get(name)
        }
      }

      super$createModuleSpecifications(analysis)
    },

    #' @description Validate the module specifications
    #'
    #' @param moduleSpecifications The CohortMethod module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  ),
  private = list(
    .getResultsDataModelSpecificationFileLocation = function() {
      return(system.file(
        file.path("csv", "treatmentPatternsRdms.csv"),
        package = "Strategus"
      ))
    }
  )
)
