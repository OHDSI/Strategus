# SelfControlledCaseSeriesModule -------------
#' @title Self-Controlled Case Series design with the \href{https://ohdsi.github.io/SelfControlledCaseSeries/}{HADES SelfControlledCaseSeries Package}
#' @export
#' @description
#' Module for performing Self-Controlled Case Series (SCCS) analyses
#' against the OMOP Common Data Model.
SelfControlledCaseSeriesModule <- R6::R6Class(
  classname = "SelfControlledCaseSeriesModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix for results tables
    tablePrefix = "sccs_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the SelfControlledCaseSeries package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      sccsMultiThreadingSettings <- SelfControlledCaseSeries::createDefaultSccsMultiThreadingSettings(jobContext$moduleExecutionSettings$maxCores)

      # Provide hook to allow for overriding the number of threads
      # used for database operations
      getDbSccsDataThreads <- as.integer(getOption("strategus.SelfControlledCaseSeriesModule.getDbSccsDataThreads"))
      fitSccsModelThreads <- as.integer(getOption("strategus.SelfControlledCaseSeriesModule.fitSccsModelThreads"))
      if (isTRUE(getDbSccsDataThreads > 0)) {
        private$.message(paste0("Detected strategus.SelfControlledCaseSeriesModule.getDbSccsDataThreads - setting value to: ", getDbSccsDataThreads))
        sccsMultiThreadingSettings$getDbSccsDataThreads <- getDbSccsDataThreads
      }
      if (isTRUE(fitSccsModelThreads > 0)) {
        private$.message(paste0("Detected strategus.SelfControlledCaseSeriesModule.fitSccsModelThreads - setting value to: ", fitSccsModelThreads))
        sccsMultiThreadingSettings$fitSccsModelThreads <- fitSccsModelThreads
      }

      # Add a check to ensure that the module specifications conform to the new
      # SCCS v6 approach
      if (is.null(jobContext$settings$sccsAnalysesSpecifications)) {
        stop("The SelfControlledCaseSeriesModule specification is missing the required `sccsAnalysesSpecifications` setting. Please recreate the SelfControlledCaseSeriesModule specification and update the analysis specification.")
      }

      args <- jobContext$settings
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
      args$tempEmulationSchema <- jobContext$moduleExecutionSettings$tempEmulationSchema
      args$exposureDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$exposureTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outcomeDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$outcomeTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$nestingCohortDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$nestingCohortTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$customCovariateDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$customCovariateTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outputFolder <- jobContext$moduleExecutionSettings$workSubFolder
      args$sccsMultiThreadingSettings <- sccsMultiThreadingSettings
      args$sccsAnalysesSpecifications <- SelfControlledCaseSeries::convertUntypedListToSccsAnalysesSpecifications(jobContext$settings$sccsAnalysesSpecifications)
      do.call(SelfControlledCaseSeries::runSccsAnalyses, args)

      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      SelfControlledCaseSeries::exportToCsv(
        outputFolder = jobContext$moduleExecutionSettings$workSubFolder,
        exportFolder = exportFolder,
        databaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount
      )
      # TODO: Removing this to make the upload easier
      # unlink(file.path(exportFolder, sprintf("Results_%s.zip", jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId)))

      resultsDataModel <- self$getResultsDataModelSpecification()
      resultsDataModel <- resultsDataModel[file.exists(file.path(exportFolder, paste0(resultsDataModel$tableName, ".csv"))), ]
      if (any(!startsWith(resultsDataModel$tableName, self$tablePrefix))) {
        stop("Table names do not have required prefix")
      }
      CohortGenerator::writeCsv(
        x = resultsDataModel,
        file = file.path(exportFolder, "resultsDataModelSpecification.csv"),
        warnOnFileNameCaseMismatch = FALSE
      )

      private$.message(paste("Results available at:", exportFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = "") {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      # Note: not passing the tablePrefix argument to
      # createResultsDataModel since the SCCS results
      # model already contains the "sccs_" table prefix
      SelfControlledCaseSeries::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsDatabaseSchema,
      )
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries"),
        warnOnCaseMismatch = FALSE
      )

      # add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      # TODO: This is something SCCS does differently.
      # Find the results zip file in the results sub folder
      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      zipFiles <- list.files(
        path = resultsFolder,
        pattern = "\\.zip$",
        full.names = TRUE
      )

      if (length(zipFiles) > 0) {
        zipFileName <- zipFiles[1]
      } else {
        # Create a zip file from the results in the directory
        DatabaseConnector::createZipFile(
          zipFile = "results.zip",
          files = list.files(resultsFolder, pattern = ".*\\.csv$"),
          rootFolder = resultsFolder
        )
        zipFileName <- file.path(resultsFolder, "results.zip")
      }

      SelfControlledCaseSeries::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        zipFileName = zipFileName,
        purgeSiteDataBeforeUploading = FALSE
      )
    },
    #' @description Creates the SelfControlledCaseSeries Module Specifications
    #' @param sccsAnalysesSpecifications An R6 class created by SelfControlledCaseSeries::createSccsAnalysesSpecifications
    #' @param sccsAnalysisList Deprecated with SelfControlledCaseSeries v6 - please use the `sccsAnalysesSpecifications` parameter instead.
    #' @param exposuresOutcomeList Deprecated with SelfControlledCaseSeries v6 - please use the `sccsAnalysesSpecifications` parameter instead.
    #' @param analysesToExclude Deprecated with SelfControlledCaseSeries v6 - please use the `sccsAnalysesSpecifications` parameter instead.
    #' @param combineDataFetchAcrossOutcomes Deprecated with SelfControlledCaseSeries v6 - please use the `sccsAnalysesSpecifications` parameter instead.
    #' @param sccsDiagnosticThresholds Deprecated with SelfControlledCaseSeries v6 - please use the `sccsAnalysesSpecifications` parameter instead.
    createModuleSpecifications = function(sccsAnalysesSpecifications,
                                          sccsAnalysisList = NULL,
                                          exposuresOutcomeList = NULL,
                                          analysesToExclude = NULL,
                                          combineDataFetchAcrossOutcomes = NULL,
                                          sccsDiagnosticThresholds = NULL) {
      paramDeprecatedMessage <- "`%s` is now part of the `sccsAnalysesSpecifications` in SelfControlledCaseSeries v6. Please upgrade to SelfControlledCaseSeries v6 and use the `sccsAnalysesSpecifications` parameter when specifying the input to this module."
      if (!is.null(sccsAnalysisList)) {
        stop(sprintf(paramDeprecatedMessage, "sccsAnalysisList"))
      }

      if (!is.null(exposuresOutcomeList)) {
        stop(sprintf(paramDeprecatedMessage, "exposuresOutcomeList"))
      }

      if (!is.null(combineDataFetchAcrossOutcomes)) {
        stop(sprintf(paramDeprecatedMessage, "combineDataFetchAcrossOutcomes"))
      }

      if (!is.null(sccsDiagnosticThresholds)) {
        stop(sprintf(paramDeprecatedMessage, "sccsDiagnosticThresholds"))
      }

      analysis <- list()
      for (name in names(formals(self$createModuleSpecifications))) {
        analysis[[name]] <- get(name)
      }

      specifications <- super$createModuleSpecifications(
        moduleSpecifications = analysis
      )
      return(specifications)
    },
    #' @description Validate the module specifications
    #' @param moduleSpecifications The SelfControlledCaseSeries module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  )
)
