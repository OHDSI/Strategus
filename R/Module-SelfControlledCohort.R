# SelfControlledCohortModule -------------
#' @title Self-Controlled Case Series design with the \href{https://ohdsi.github.io/SelfControlledCohort/}{HADES SelfControlledCohort Package}
#' @export
#' @description
#' Module for performing Self-Controlled Cohort (SCC) analyses
#' against the OMOP Common Data Model.
SelfControlledCohortModule <- R6::R6Class(
  classname = "SelfControlledCohortModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix for results tables
    tablePrefix = "scc_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the SelfControlledCohort package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      sccMultiThreadingSettings <- SelfControlledCohort::createDefaultSccMultiThreadingSettings(jobContext$moduleExecutionSettings$maxCores)

      args <- jobContext$settings
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
      args$exposureTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outcomeDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$exposureDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$outcomeTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outputFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      args$sccAnalysisList <- jobContext$moduleExecutionSettings$sccAnalysisList
      args$exposureOutcomeList <- jobContext$moduleExecutionSettings$exposureOutcomeList

      args$analysisThreads <- jobContext$moduleExecutionSettings$analysisThreads
      args$computeThreads <- jobContext$moduleExecutionSettings$computeThreads

      args$sccDiagnosticThresholds <- NULL
      do.call(SelfControlledCohort::runSccAnalyses, args)




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
      # model already contains the "scc_" table prefix
      SelfControlledCohort::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsDatabaseSchema,
        tablePrefix = tablePrefix
      )
    },
    #' @description Get the results data model specification for the module
    getResultsDataModelSpecification = function(...) {
      resultsDataModelSpecification <- SelfControlledCohort::getResultsDataModelSpecifications()
      return(resultsDataModelSpecification)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      SelfControlledCohort::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        zipFileName = zipFileName,
        purgeSiteDataBeforeUploading = FALSE
      )
    },
    #' @description Creates the SelfControlledCohort Module Specifications
    #' @param sccAnalysisList see SelfControlledCohort::createSccAnalysis and SelfControlledCohort::createRunSelfControlledCohortArgs
    #' @param exposuresOutcomeList See SelfControlledCohort::createExposureOutcome
    createModuleSpecifications = function(sccAnalysisList,
                                          exposuresOutcomeList) {
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
    #' @param moduleSpecifications The SelfControlledCohort module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  )
)
