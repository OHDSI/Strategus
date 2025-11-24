# SelfControlledCohort -------------
#' @title Self-Controlled Case Series design with the \href{https://ohdsi.github.io/SelfControlledCaseSeries/}{HADES SelfControlledCaseSeries Package}
#' @export
#' @description
#' Module for performing Self-Controlled Cohort (SCC) analyses
#' against the OMOP Common Data Model.
SelfControlledCohort <- R6::R6Class(
  classname = "SelfControlledCohort",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix for results tables
    tablePrefix = "scc_",
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
      computeThreads <- as.integer(getOption("strategus.SelfControlledCohort.threads"))

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
      do.call(SelfControlledCaseSeries::execute, args)
      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

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
      SelfControlledCohort::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsDatabaseSchema,
        tablePrefix = tablePrefix
      )
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- SelfControlledCohort::getResultsDataModelSpecifications()
      # add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings, ...) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      SelfControlledCohort::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        ...
      )
    },
    #' @description Creates the SelfControlledCaseSeries Module Specifications
    #' @param sccAnalysesSpecifications An R6 class created by SelfControlledCohort::createSccAnalysis
    createModuleSpecifications = function(sccAnalysesSpecifications) {
      specifications <- super$createModuleSpecifications(
        moduleSpecifications = sccAnalysesSpecifications
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
