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
      analysisList <- spec$tpAnalysisList

      if (is.null(analysisList)) {
        settings <- list()
        for (setting in names(spec)) {
          settings[[setting]] <- spec[[setting]]
        }

        # new parameters(cross compatibility)
        if (is.null(spec$analysisId)) {
          settings[["analysisId"]] <- 1
        }
        if (is.null(spec$description)) {
          settings[["description"]] <- ""
        }
        if (is.null(spec$stratify)) {
          settings[["stratify"]] <- FALSE
        }
        analysisList <- list(settings)
      }

      errorMessages <- character()
      env <- environment()


      # checks if there are failed pathways and runs only those; if none reruns all analyses
      resultAppend <- FALSE

      passedPathways <- integer(0)
      if (isTRUE(executionSettings$incremental) && file.exists(file.path(workFolder, "passed_pathway_runs.csv")) && dir.exists(resultsFolder)) {
        passedDf <- readr::read_csv(
          file = file.path(workFolder, "passed_pathway_runs.csv"),
          show_col_types = FALSE,
          col_types = readr::cols_only(analysis_id = readr::col_integer())
        )

        passedPathways <- as.integer(passedDf$analysis_id)

        file.remove(file.path(workFolder, "failed_pathway_runs.csv"))

        resultAppend <- TRUE
      }

      pathwayResult <- NULL

      for (idx in seq_along(analysisList)) {
        if (!is.null(analysisList[[idx]]$settings)) {
          analysis <- analysisList[[idx]]$settings
        } else {
          analysis <- analysisList[[idx]]
        }

        analysisId <- analysis$analysisId

        if (length(passedPathways) > 0 && analysisId %in% passedPathways) {
          private$.message(paste("Analysis", analysisId, "pathway already constructed"))
        } else {
          cohorts <- super$.listToDataFrame(analysis$cohorts)

          targets <- cohorts[cohorts$type == "target", c("cohortName", "cohortId"), drop = FALSE]

          cohortAnalysisTable <- cohorts %>% dplyr::mutate("analysisId" = analysisId)
          tryCatch(
            {
              outputEnv <- TreatmentPatterns::computePathways(
                analysisId = analysisId,
                description = analysis$description,
                cohorts = cohorts,
                cohortTableName = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
                connectionDetails = connectionDetails,
                cdmSchema = executionSettings$cdmDatabaseSchema,
                resultSchema = executionSettings$workDatabaseSchema,
                tempEmulationSchema = executionSettings$tempEmulationSchema,
                startAnchor = analysis$startAnchor,
                windowStart = analysis$windowStart,
                endAnchor = analysis$endAnchor,
                windowEnd = analysis$windowEnd,
                minEraDuration = analysis$minEraDuration,
                splitEventCohorts = analysis$splitEventCohorts,
                splitTime = analysis$splitTime,
                eraCollapseSize = analysis$eraCollapseSize,
                combinationWindow = analysis$combinationWindow,
                minPostCombinationDuration = analysis$minPostCombinationDuration,
                filterTreatments = analysis$filterTreatments,
                maxPathLength = analysis$maxPathLength,
                overlapMethod = analysis$overlapMethod,
                concatTargets = analysis$concatTargets
              )

              result <- TreatmentPatterns::export(
                andromeda = outputEnv,
                outputPath = NULL,
                ageWindow = analysis$ageWindow,
                minCellCount = executionSettings$minCellCount,
                censorType = analysis$censorType,
                stratify = analysis$stratify,
                archiveName = NULL
              )

              if (is.null(pathwayResult)) {
                pathwayResult <- Andromeda::andromeda(
                  attrition = result$attrition,
                  metadata = result$metadata,
                  treatmentPathways = result$treatment_pathways,
                  summaryEventDuration = result$summary_event_duration,
                  countsAge = result$counts_age,
                  countsSex = result$counts_sex,
                  countsYear = result$counts_year,
                  cdmSourceInfo = result$cdm_source_info,
                  analyses = result$analyses,
                  arguments = result$arguments,
                  analysisCohorts = cohortAnalysisTable
                )
              } else {
                Andromeda::appendToTable(pathwayResult$attrition, result$attrition)
                Andromeda::appendToTable(pathwayResult$metadata, result$metadata)
                Andromeda::appendToTable(pathwayResult$treatmentPathways, result$treatment_pathways)
                Andromeda::appendToTable(pathwayResult$summaryEventDuration, result$summary_event_duration)
                Andromeda::appendToTable(pathwayResult$countsAge, result$counts_age)
                Andromeda::appendToTable(pathwayResult$countsSex, result$counts_sex)
                Andromeda::appendToTable(pathwayResult$countsYear, result$counts_year)
                Andromeda::appendToTable(pathwayResult$cdmSourceInfo, result$cdm_source_info)
                Andromeda::appendToTable(pathwayResult$analyses, result$analyses)
                Andromeda::appendToTable(pathwayResult$arguments, result$arguments)
                Andromeda::appendToTable(pathwayResult$analysisCohorts, cohortAnalysisTable)
              }

              private$.errorHandler(file = file.path(workFolder, "passed_pathway_runs.csv"), analysisId = analysisId, targets = targets, message = "passed")
            },
            error = function(err) {
              msg <- sprintf("Analysis '%s' pathway construction failed: %s", analysisId, conditionMessage(err))
              private$.message(msg)

              current <- get("errorMessages", envir = env)
              assign("errorMessages", c(current, msg), envir = env)

              private$.errorHandler(file = file.path(workFolder, "failed_pathway_runs.csv"), analysisId = analysisId, targets = targets, message = msg)
            }
          )
        }
      }

      if (isFALSE(resultAppend)){
        unlink(resultsFolder, recursive = TRUE)
        dir.create(resultsFolder, recursive = TRUE, showWarnings = FALSE)
      }

      # writes the results to csv
      for (name in names(pathwayResult)) {
        data <- pathwayResult[[name]] %>% dplyr::collect()
        snakeCaseName = SqlRender::camelCaseToSnakeCase(name)

        if(name == "analysisCohorts"){
          colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
        }

        readr::write_csv(x = data, file = file.path(resultsFolder, paste0(self$tablePrefix, snakeCaseName, ".csv")), append = resultAppend)
      }

      # HACK: Append the database_id to all exported results
      csvFiles <- list.files(resultsFolder, pattern = "\\.csv$", full.names = TRUE)
      for (file in csvFiles) {
        if (!(tools::file_path_sans_ext(basename(file)) %in% c(paste0(self$tablePrefix, "analyses"), paste0(self$tablePrefix, "analysis_cohorts")))) {
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


      # Export the resultsDataModelSpecification.csv
      resultsDataModelSpecification <- self$getResultsDataModelSpecification()

      CohortGenerator::writeCsv(
        x = resultsDataModelSpecification,
        file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
        warnOnCaseMismatch = FALSE,
        warnOnFileNameCaseMismatch = FALSE,
        warnOnUploadRuleViolations = FALSE
      )

      if (length(errorMessages) > 0) {
        msg <- sprintf(
          "Module failed: %d analysis(es) failed. See logs and per-analysis artifacts in '%s' (failed_pathway_runs.csv). Errors: '%s'",
          length(errorMessages),
          workFolder,
          paste(errorMessages, collapse = "; ")
        )
        stop(msg)
      }

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
    #' @param analysisId (`numeric(1)`)
    #' Unique identifier for the TreatmentPatterns analysis
    #' @param cohorts (`data.frame()`)\cr
    #' Data frame containing the following columns and data types:
    #' \describe{
    #'  \item{cohortId `numeric(1)`}{Cohort ID's of the cohorts to be used in the cohort table.}
    #'  \item{cohortName `character(1)`}{Cohort names of the cohorts to be used in the cohort table.}
    #'  \item{type `character(1)` \["target", "event', "exit"\]}{Cohort type, describing if the cohort is a target, event, or exit cohort}
    #' }
    #' @param description (`character(1)`)
    #' Description for analysis
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
    #' @param stratify (`logical(1)`)\cr
    #' This will perform pairwise stratification between age, sex, and index year if set TRUE
    createModuleSpecifications = function(analysisId = 1,
                                          cohorts,
                                          description = "",
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
                                          windowEnd = 0,
                                          stratify = FALSE) {
      if (!is.null(indexDateOffset)) {
        warning("`indexDateOffset` is deprecated in TreatmentPatterns 3.1.0, please use: `startAnchor`, `windowStart`, `endAnchor`, `windowEnd` instead.")
      }

      if (!is.null(includeTreatments)) {
        warning("`includeTreatments` is deprecated in TreatentPatterns 3.1.0, please use: `startAnchor`, `windowStart`, `endAnchor`, `windowEnd` instead.")
      }

      # validate user input
      for (cohort in cohorts$cohortName) {
        tryCatch(
          {
            # Attempt to compile the regex by running grepl on an empty string
            grepl(cohort, "", perl = TRUE)
            if (grepl("[-+]", cohort, perl = TRUE)) {
              stop(sprintf("Invalid target cohort name; remove -/+"))
            }
          },
          error = function(e) {
            stop(
              sprintf(
                "Invalid target cohort name '%s': %s",
                cohort, e$message
              ),
              call. = FALSE
            )
          }
        )
      }

      analysis <- list()
      analysis[["cohorts"]] <- super$.dataFrameToList(cohorts)

      for (name in names(formals(self$createModuleSpecifications))) {
        if (!(name %in% c("cohorts"))) {
          analysis[[name]] <- get(name)
        }
      }

      specification <- super$createModuleSpecifications(analysis)

      return(specification)
    },
    #' @description
    #' Runs multiple analyses using a list of analysis specification objects (as produced by\code{createModuleSpecifications}) into a single module specification
    #'
    #' @param tpAnalysisList (`list()`)\cr
    #' A list of analysis specification objects.
    #' Each element should be a list created by \code{createAnalysisSpecification}
    createMultiAnalysisModuleSpecification = function(tpAnalysisList) {
      analysisIds <- list()
      for (analysis in tpAnalysisList) {
        analysisIds[[length(analysisIds) + 1]] <- analysis$settings$analysisId
      }

      if (anyDuplicated(analysisIds)) {
        stop("Each analysis need unique id")
      }

      specification <- super$createModuleSpecifications(
        moduleSpecifications = list(tpAnalysisList = tpAnalysisList)
      )
      return(specification)
    },
    #' @description Validate the module specifications
    #'
    #' @param moduleSpecifications The treatment patterns module specifications
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
    },
    .errorHandler = function(file, analysisId, targets, message) {
      if (file.exists(file)) {
        append <- TRUE
      } else {
        append <- FALSE
      }

      error <- data.frame(
        analysis_id = analysisId,
        target_names = paste(targets$target_cohort_name, collapse = ";"),
        target_ids = paste(targets$target_cohort_id, collapse = ";"),
        error = message,
        timestamp = Sys.time(),
        stringsAsFactors = FALSE
      )

      readr::write_csv(x = error, file = file, append = append)
    }
  )
)
