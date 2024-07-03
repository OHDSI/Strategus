# CohortMethodModule -------------
#' @title Module for performing new-user cohort studies
#' @export
#' @description
#' Module for performing new-user cohort studies in an observational
#' database in the OMOP Common Data Model.
CohortMethodModule <- R6::R6Class(
  classname = "CohortMethodModule",
  inherit = StrategusModule,
  public = list(
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the CohortMethod packages
    #' @param connectionDetails The connection details to the database
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param executionSettings The execution settings for the study
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      jobContext <- private$jobContext

      multiThreadingSettings <- CohortMethod::createDefaultMultiThreadingSettings(parallel::detectCores())

      args <- jobContext$settings
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
      args$exposureDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$exposureTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outcomeDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$outcomeTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outputFolder <- jobContext$moduleExecutionSettings$workSubFolder
      args$multiThreadingSettings <- multiThreadingSettings
      args$cmDiagnosticThresholds <- NULL
      do.call(CohortMethod::runCmAnalyses, args)

      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      CohortMethod::exportToCsv(
        outputFolder = jobContext$moduleExecutionSettings$workSubFolder,
        exportFolder = exportFolder,
        databaseId = jobContext$moduleExecutionSettings$databaseId,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        maxCores = parallel::detectCores(),
        cmDiagnosticThresholds = jobContext$settings$cmDiagnosticThresholds
      )
      # TODO: Removing this to make the upload easier
      #unlink(file.path(exportFolder, sprintf("Results_%s.zip", jobContext$moduleExecutionSettings$databaseId)))

      resultsDataModel <- CohortGenerator::readCsv(file = system.file("csv", "resultsDataModelSpecification.csv", package = "CohortMethod"))
      CohortGenerator::writeCsv(
        x = resultsDataModel,
        file = file.path(exportFolder, "resultsDataModelSpecification.csv"),
        warnOnFileNameCaseMismatch = FALSE
      )
      private$.message(paste("Results available at:", exportFolder))
      private$.clearLoggers()
    },
    #' @description Create the results schema for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param resultsSchema The schema holding the results
    #' @param tablePrefix The prefix to use to append to the results tables (optional)
    createResultsSchema = function(resultsConnectionDetails, resultsSchema, tablePrefix = "") {
      super$createResultsSchema(resultsConnectionDetails, resultsSchema, tablePrefix)
      CohortMethod::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsSchema,
        tablePrefix = tablePrefix
      )
    },
    #' @description Upload the results for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param resultsExecutionSettings The results execution settings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings)

      # TODO: This is something CM does differently.
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

      # TODO: The decisions to set the parameters:
      #    forceOverWriteOfSpecifications = FALSE
      #    purgeSiteDataBeforeUploading = FALSE
      # needs discussion.
      CohortMethod::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsExecutionSettings$resultsDatabaseSchema,
        zipFileName = zipFileName,
        forceOverWriteOfSpecifications = FALSE,
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
    #' [getFileReference()] function. A summary of the analysis results can be obtained using the
    #' [getResultsSummary()] function.
    #'
    #' ## Analyses to Exclude
    #'
    #' Normally, `runCmAnalyses` will run all combinations of target-comparator-outcome-analyses settings.
    #' However, sometimes we may not need all those combinations. Using the `analysesToExclude` argument,
    #' we can remove certain items from the full matrix. This argument should be a data frame with at least
    #' one of the following columns:
    #'
    #' @param cmAnalysisList                 A list of objects of type `cmAnalysis` as created using
    #'                                       the `[CohortMethod::createCmAnalysis] function.
    #' @param targetComparatorOutcomesList   A list of objects of type `targetComparatorOutcomes` as
    #'                                       created using the [CohortMethod::createTargetComparatorOutcomes]
    #'                                       function.
    #' @param analysesToExclude              Analyses to exclude. See the Analyses to Exclude section for details.
    #' @param refitPsForEveryOutcome         Should the propensity model be fitted for every outcome (i.e.
    #'                                       after people who already had the outcome are removed)? If
    #'                                       false, a single propensity model will be fitted, and people
    #'                                       who had the outcome previously will be removed afterwards.
    #' @param refitPsForEveryStudyPopulation Should the propensity model be fitted for every study population
    #'                                       definition? If false, a single propensity model will be fitted,
    #'                                       and the study population criteria will be applied afterwards.
    #' @param cmDiagnosticThresholds An object of type `CmDiagnosticThresholds` as created using
    #'                                 [CohortMethod::createCmDiagnosticThresholds()].
    #'
    createModuleSpecifications = function(cmAnalysisList,
                                          targetComparatorOutcomesList,
                                          analysesToExclude = NULL,
                                          refitPsForEveryOutcome = FALSE,
                                          refitPsForEveryStudyPopulation = TRUE,
                                          cmDiagnosticThresholds = createCmDiagnosticThresholds()) {
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
    #' @param moduleSpecifications The CohortIncidence module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  )
)
