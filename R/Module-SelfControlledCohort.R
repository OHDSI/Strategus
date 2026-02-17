# SelfControlledCohort -------------
#' @title Self-Controlled Cohort design with the \href{https://ohdsi.github.io/SelfControlledCohort/}{HADES SelfControlledCohort Package}
#' @export
#' @description
#' Module for performing Self-Controlled Cohort (SCC) analyses
#' against the OMOP Common Data Model.
#'
#' @details
#' This R6 class acts as a thin wrapper around the SelfControlledCohort package functions.
#' The design pattern used here delegates all implementation logic to the package itself,
#' which means this wrapper should rarely (ideally never) need to change when the underlying
#' package evolves. This approach reduces tight coupling between Strategus and analysis modules,
#' making the system more maintainable and allowing module developers to update their packages
#' independently without requiring changes to the Strategus framework.
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

    #' @description Executes the SelfControlledCohort package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    #'
    #' @details
    #' This method simply constructs a jobContext object and delegates execution
    #' to the package's own execute() function. By keeping this wrapper minimal,
    #' we avoid the need to update Strategus every time the underlying analysis
    #' logic changes. The package itself owns its execution logic, version checking,
    #' and parameter handling - this wrapper just provides the interface contract
    #' that Strategus expects.
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      # Construct jobContext in the format expected by the package's execute() function
      # This is the only coupling point - the jobContext structure. As long as we
      # maintain this interface, the package can change its internal implementation freely.
      jobContext <- list(
        connectionDetails = connectionDetails,
        executionSettings = private$jobContext$moduleExecutionSettings,
        moduleExecutionSettings = analysisSpecifications
      )

      # Delegate to the package's execute function
      # The package handles all logic including version checking, parameter validation,
      # and actual analysis execution. This wrapper doesn't need to know about those details.
      SelfControlledCohort::execute(jobContext)

      exportFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder

      # Handle results metadata - this is framework-level concern, not package concern
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
    #'
    #' @details
    #' Delegates to the package's own createResultsDataModel function.
    #' The package knows its own schema requirements - Strategus just needs to
    #' call the function with the appropriate database connection details.
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = "") {
      # Direct delegation - no need to replicate schema logic here
      SelfControlledCohort::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsDatabaseSchema,
        tablePrefix = tablePrefix
      )
    },

    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    #'
    #' @details
    #' Retrieves the data model specification from the package itself.
    #' Only framework-specific modification is adding the table prefix.
    getResultsDataModelSpecification = function(tablePrefix = "") {
      # Get specification from package - it knows its own schema
      resultsDataModelSpecification <- SelfControlledCohort::getResultsDataModelSpecifications()
      # Apply framework-level prefix requirement
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },

    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    #'
    #' @details
    #' Delegates upload logic to the package's uploadResults function.
    #' The package handles the specifics of what data needs to be uploaded and how.
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings, ...) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      # Direct delegation to package function
      SelfControlledCohort::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        ...
      )
    },

    #' @description Creates the SelfControlledCohort Module Specifications
    #' @param sccAnalysesSpecifications Module specifications created by
    #'   SelfControlledCohort::createSelfControlledCohortModuleSpecifications()
    #'
    #' @details
    #' Wraps the package's createModuleSpecifications output in Strategus format.
    #' The package function already handles versioning, parameter validation, etc.
    #' This wrapper just ensures compatibility with Strategus conventions.
    createModuleSpecifications = function(sccAnalysesSpecifications) {
      # The package creates its own specifications with proper version metadata
      # We just need to ensure it's wrapped in the parent class structure
      specifications <- super$createModuleSpecifications(
        moduleSpecifications = sccAnalysesSpecifications
      )
      return(specifications)
    },

    #' @description Validate the module specifications
    #' @param moduleSpecifications The SelfControlledCohort module specifications
    #'
    #' @details
    #' Validation happens within the package's execute() function (including version checking).
    #' This method provides the Strategus interface but doesn't duplicate validation logic.
    validateModuleSpecifications = function(moduleSpecifications) {
      # Parent class validation handles framework-level requirements
      # Package-specific validation (including version compatibility) happens
      # in the package's execute() function via checkModuleVersion()
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  )
)
