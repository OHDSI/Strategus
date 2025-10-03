# ComparatorSelectionExplorerModule -------------
#' @title Analyze comparator selection using the ComparatorSelectionExplorer package
#' @export
#' @description
#' Development and evaluation of comparator selection algorithms
#' against the OMOP Common Data Model.
ComparatorSelectionExplorerModule <- R6::R6Class(
  classname = "ComparatorSelectionExplorerModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix to append to results tables
    tablePrefix = "cse_",

    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },

    #' @description Executes the ComparatorSelectionExplorer package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      args <- jobContext$settings
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
      args$vocabularyDatabaseSchema <- jobContext$moduleExecutionSettings$vocabularyDatabaseSchema
      args$resultsDatabaseSchema <- jobContext$moduleExecutionSettings$resultsDatabaseSchema
      args$cohortDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$tempEmulationSchema <- jobContext$moduleExecutionSettings$tempEmulationSchema
      args$cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext()
      args$exportZipFile <- file.path(exportFolder, "results.zip")
      args$incrementalFolder <- jobContext$moduleExecutionSettings$workSubFolder
      args$logFileLocation <- file.path(exportFolder, "execution-log.txt")
      args$generateCohortDefinitionSet <- TRUE

      # Execute the ComparatorSelectionExplorer package
      ComparatorSelectionExplorer::execute(executionSettings = do.call(ComparatorSelectionExplorer::createExecutionSettings, args))

      private$.message(paste("Results available at:", exportFolder))
    },

    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      # Custom results data model creation logic if needed
      ComparatorSelectionExplorer::createResultsDataModel(connectionDetails = resultsConnectionDetails,
                                                          databaseSchema = resultsDatabaseSchema,
                                                          tablePrefix = tablePrefix)
    },

    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- private$.getResultsDataModelSpecification()
      # Add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, self$tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },

    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      zipFiles <- list.files(
        path = resultsFolder,
        pattern = "\\.zip$",
        full.names = TRUE
      )

      if (length(zipFiles) > 0) {
        zipFileName <- zipFiles[1]
      } else {
        DatabaseConnector::createZipFile(
          zipFile = "results.zip",
          files = list.files(resultsFolder, pattern = ".*\\.csv$"),
          rootFolder = resultsFolder
        )
        zipFileName <- file.path(resultsFolder, "results.zip")
      }

      # Upload results using ComparatorSelectionExplorer
      ComparatorSelectionExplorer::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        tablePrefix = self$tablePrefix,
        zipFileName = zipFileName
      )
    },

    #' @description Creates the ComparatorSelectionExplorer Module Specifications
    #' @param ... Additional arameters for ComparatorSelectionExplorer settings
    #' @param minExposureSize minimum number of individuals exposed to be considered in this analysis
    #' @param targetCohortIds (optional) subset of cohort definition set to compute all pairwise comparisons for. By the N x N matrix of all similarity scores will be computed.
    createModuleSpecifications = function(minExposureSize = 1000, targetCohortIds = NULL, ...) {
      checkmate::assertIntegerish(minExposureSize, len = 1, lower = 1)
      checkmate::assertNumeric(targetCohortIds, min.len = 1, null.ok = TRUE)
      if (!is.null(targetCohortIds))
        checkmate::assertTRUE(all(targetCohortIds %% 1 == 0))
      analysis <- list(minExposureSize = minExposureSize, targetCohortIds = targetCohortIds, ...)
      specifications <- super$createModuleSpecifications(moduleSpecifications = analysis)
      return(specifications)
    },

    #' @description Validate the module specifications
    #' @param moduleSpecifications The ComparatorSelectionExplorer module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(moduleSpecifications = moduleSpecifications)
    }
  ),

  private = list(
    .getResultsDataModelSpecification = function() {
      ComparatorSelectionExplorer::getResultsDataModelSpec()
    }
  )
)
