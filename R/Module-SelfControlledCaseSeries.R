# SelfControlledCaseSeriesModule -------------
#' @title Module for performing Self-Controlled Case Series (SCCS) analyses
#' in an observational database in the OMOP Common Data Model.
#' @export
#' @description
#' Module for performing Self-Controlled Case Series (SCCS) analyses
#' in an observational database in the OMOP Common Data Model.
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
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      checkmate::assertClass(executionSettings, "CdmExecutionSettings")

      jobContext <- private$jobContext
      sccsMultiThreadingSettings <- SelfControlledCaseSeries::createDefaultSccsMultiThreadingSettings(parallel::detectCores())

      args <- jobContext$settings
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
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
      args$sccsDiagnosticThresholds <- NULL
      do.call(SelfControlledCaseSeries::runSccsAnalyses, args)

      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      SelfControlledCaseSeries::exportToCsv(
        outputFolder = jobContext$moduleExecutionSettings$workSubFolder,
        exportFolder = exportFolder,
        databaseId = jobContext$moduleExecutionSettings$databaseId,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        sccsDiagnosticThresholds = jobContext$settings$sccsDiagnosticThresholds
      )
      # TODO: Removing this to make the upload easier
      #unlink(file.path(exportFolder, sprintf("Results_%s.zip", jobContext$moduleExecutionSettings$databaseId)))

      resultsDataModel <- CohortGenerator::readCsv(file = system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries"))
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
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsUploadSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsUploadSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsUploadSettings)

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

      # TODO: This function does not expose
      # a way to specify the database identifier file
      # which makes the purge problematic since I'm
      # not sure how it will know what to purge...
      SelfControlledCaseSeries::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsUploadSettings$resultsDatabaseSchema,
        zipFileName = zipFileName,
        purgeSiteDataBeforeUploading = resultsUploadSettings$purgeSiteDataBeforeUploading
      )
    },
    #' @description Creates the SelfControlledCaseSeries Module Specifications
    #' @param sccsAnalysisList description
    #' @param exposuresOutcomeList description
    #' @param analysesToExclude description
    #' @param combineDataFetchAcrossOutcomes description
    #' @param sccsDiagnosticThresholds description
    createModuleSpecifications = function(sccsAnalysisList,
                                          exposuresOutcomeList,
                                          analysesToExclude = NULL,
                                          combineDataFetchAcrossOutcomes = FALSE,
                                          sccsDiagnosticThresholds = SelfControlledCaseSeries::createSccsDiagnosticThresholds()) {
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
