# CohortMethodModule -------------
#' @title New-user cohort studies with the \href{https://ohdsi.github.io/CohortMethod/}{HADES CohortMethod Package}
#' @export
#' @description
#' Module for performing new-user cohort studies against
#' the OMOP Common Data Model
CohortMethodModule <- R6::R6Class(
  classname = "CohortMethodModule",
  inherit = StrategusModule,
  public = list(
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the CohortMethod package
    #' @template connectionDetails
    #' @param analysisSpecifications The analysis specifications for the study
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      multiThreadingSettings <- CohortMethod::createDefaultMultiThreadingSettings(jobContext$moduleExecutionSettings$maxCores)

      # Provide hook to allow for overriding the number of threads
      # used for database operations
      getDbCohortMethodDataThreads <- as.integer(getOption("strategus.CohortMethodModule.getDbCohortMethodDataThreads"))
      fitOutcomeModelThreads <- as.integer(getOption("strategus.CohortMethodModule.fitOutcomeModelThreads"))
      if (isTRUE(getDbCohortMethodDataThreads > 0)) {
        private$.message(paste0("Detected strategus.CohortMethodModule.getDbCohortMethodDataThreads - setting value to: ", getDbCohortMethodDataThreads))
        multiThreadingSettings$getDbCohortMethodDataThreads <- getDbCohortMethodDataThreads
      }
      if (isTRUE(fitOutcomeModelThreads > 0)) {
        private$.message(paste0("Detected strategus.CohortMethodModule.fitOutcomeModelThreads - setting value to: ", fitOutcomeModelThreads))
        multiThreadingSettings$fitOutcomeModelThreads <- fitOutcomeModelThreads
      }

      # Add a check to ensure that the module specifications conform to the new
      # CM v6 approach
      if (is.null(jobContext$settings$cmAnalysesSpecifications)) {
        stop("The CohortMethodModule specification is missing the required `cmAnalysesSpecifications` setting. Please recreate the CohortMethodModule specification and update the analysis specification.")
      }

      args <- jobContext$settings
      args$databaseId <- jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
      args$tempEmulationSchema <- jobContext$moduleExecutionSettings$tempEmulationSchema
      args$exposureDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$exposureTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outcomeDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$outcomeTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$nestingCohortDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$nestingCohortTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outputFolder <- jobContext$moduleExecutionSettings$workSubFolder
      args$multiThreadingSettings <- multiThreadingSettings
      args$cmAnalysesSpecifications <- CohortMethod::convertUntypedListToCmAnalysesSpecifications(jobContext$settings$cmAnalysesSpecifications)
      do.call(CohortMethod::runCmAnalyses, args)

      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      CohortMethod::exportToCsv(
        outputFolder = jobContext$moduleExecutionSettings$workSubFolder,
        exportFolder = exportFolder,
        databaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        maxCores = jobContext$moduleExecutionSettings$maxCores
      )
      # TODO: Removing this to make the upload easier
      # unlink(file.path(exportFolder, sprintf("Results_%s.zip", jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId)))

      resultsDataModelSpecification <- self$getResultsDataModelSpecification()
      CohortGenerator::writeCsv(
        x = resultsDataModelSpecification,
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
      CohortMethod::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsDatabaseSchema,
        tablePrefix = tablePrefix
      )
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = system.file(
          file.path("csv", "resultsDataModelSpecification.csv"),
          package = "CohortMethod"
        ),
        warnOnCaseMismatch = FALSE
      )

      # add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      # TODO: This is something CM does differently.
      # Find the results zip file in the results sub folder
      resultsFolder <- normalizePath(private$jobContext$moduleExecutionSettings$resultsSubFolder)
      zipFiles <- list.files(
        path = resultsFolder,
        pattern = "\\.zip$",
        full.names = TRUE
      )

      if (length(zipFiles) > 0) {
        zipFileName <- zipFiles[1]
      } else {
        # Create a zip file from the results in the directory
        oldWd <- setwd(resultsFolder)
        on.exit(setwd(oldWd))
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
      CohortMethod::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        zipFileName = zipFileName,
        purgeSiteDataBeforeUploading = FALSE
      )
    },
    #' @description Creates the CohortMethod Module Specifications
    #'
    #' @details
    #' Run a list of analyses for the target-comparator-outcomes of interest. This function will run all
    #' specified analyses against all hypotheses of interest, meaning that the total number of outcome
    #' models is `length(cmAnalysisList) * length(targetComparatorOutcomesList)` (if all analyses specify an
    #' outcome model should be fitted). When you provide several analyses it will determine whether any of
    #' the analyses have anything in common, and will take advantage of this fact. For example, if we
    #' specify several analyses that only differ in the way the outcome model is fitted, then this
    #' function will extract the data and fit the propensity model only once, and re-use this in all the
    #' analysis.
    #'
    #' After completion, a tibble containing references to all generated files can be obtained using the
    #' [CohortMethod::getFileReference()] function. A summary of the analysis results can be obtained using the
    #' [CohortMethod::getResultsSummary()] function.
    #'
    #' ## Analyses to Exclude
    #'
    #' Normally, `runCmAnalyses` will run all combinations of target-comparator-outcome-analyses settings.
    #' However, sometimes we may not need all those combinations. Using the `analysesToExclude` argument,
    #' we can remove certain items from the full matrix. This argument should be a data frame with at least
    #' one of the following columns:
    #'
    #' @param cmAnalysesSpecifications An R6 class created by CohortMethod::createCmAnalysesSpecifications
    #' @param cmAnalysisList Deprecated with CohortMethod v6 - please use the `cmAnalysesSpecifications` parameter instead.
    #' @param targetComparatorOutcomesList Deprecated with CohortMethod v6 - please use the `cmAnalysesSpecifications` parameter instead.
    #' @param analysesToExclude Deprecated with CohortMethod v6 - please use the `cmAnalysesSpecifications` parameter instead.
    #' @param refitPsForEveryOutcome Deprecated with CohortMethod v6 - please use the `cmAnalysesSpecifications` parameter instead.
    #' @param refitPsForEveryStudyPopulation Deprecated with CohortMethod v6 - please use the `cmAnalysesSpecifications` parameter instead.
    #' @param cmDiagnosticThresholds Deprecated with CohortMethod v6 - please use the `cmAnalysesSpecifications` parameter instead.
    createModuleSpecifications = function(cmAnalysesSpecifications,
                                          cmAnalysisList = NULL,
                                          targetComparatorOutcomesList = NULL,
                                          analysesToExclude = NULL,
                                          refitPsForEveryOutcome = NULL,
                                          refitPsForEveryStudyPopulation = NULL,
                                          cmDiagnosticThresholds = NULL) {
      paramDeprecatedMessage <- "`%s` is now part of the `cmAnalysesSpecifications` in CohortMethod v6. Please upgrade to CohortMethod v6 and use the `cmAnalysesSpecifications` parameter when specifying the input to this module."
      if (!is.null(cmAnalysisList)) {
        stop(sprintf(paramDeprecatedMessage, "cmAnalysisList"))
      }

      if (!is.null(targetComparatorOutcomesList)) {
        stop(sprintf(paramDeprecatedMessage, "targetComparatorOutcomesList"))
      }

      if (!is.null(refitPsForEveryOutcome)) {
        stop(sprintf(paramDeprecatedMessage, "refitPsForEveryOutcome"))
      }

      if (!is.null(refitPsForEveryStudyPopulation)) {
        stop(sprintf(paramDeprecatedMessage, "refitPsForEveryStudyPopulation"))
      }

      if (!is.null(cmDiagnosticThresholds)) {
        stop(sprintf(paramDeprecatedMessage, "cmDiagnosticThresholds"))
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
    #' @param moduleSpecifications The CohortMethod module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  )
)
