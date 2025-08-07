# CohortSurvivalModule -------------
#' @title Cohort survival analysis with the \href{https://github.com/darwin-eu/CohortSurvival}{CohortSurvival Package}
#' @export
#' @description
#' Module for performing cohort survival analysis in observational
#' databases in the OMOP Common Data Model using the CohortSurvival package.
CohortSurvivalModule <- R6::R6Class(
  classname = "CohortSurvivalModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix to append to the results tables
    tablePrefix = "cohort_survival_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the CohortSurvival package
    #' @template connectionDetails
    #' @param analysisSpecifications The analysis specifications for the study
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      # Create CDM object for CohortSurvival
      cdm <- CohortSurvival::cdmFromCon(
        con = DatabaseConnector::connect(connectionDetails),
        cdmSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        writeSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTables = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )

      # Get settings from job context
      settings <- jobContext$settings
      
      # Run cohort survival analyses based on settings
      if (settings$analysisType == "single_event") {
        # Single event cohort survival analysis
        survivalResults <- CohortSurvival::estimateSingleEventSurvival(
          cdm = cdm,
          targetCohortTable = settings$targetCohortTable,
          outcomeCohortTable = settings$outcomeCohortTable,
          strata = settings$strata,
          timeGap = settings$timeGap,
          followUp = settings$followUp,
          minCellCount = jobContext$moduleExecutionSettings$minCellCount
        )
      } else if (settings$analysisType == "competing_risk") {
        # Competing risk cohort survival analysis
        survivalResults <- CohortSurvival::estimateCompetingRiskSurvival(
          cdm = cdm,
          targetCohortTable = settings$targetCohortTable,
          outcomeCohortTable = settings$outcomeCohortTable,
          competingOutcomeCohortTable = settings$competingOutcomeCohortTable,
          strata = settings$strata,
          timeGap = settings$timeGap,
          followUp = settings$followUp,
          minCellCount = jobContext$moduleExecutionSettings$minCellCount
        )
      } else {
        stop("Invalid analysis type. Must be 'single_event' or 'competing_risk'")
      }

      private$.message("Export data to csv files")

      # Export results to CSV
      CohortSurvival::exportSurvivalResults(
        x = survivalResults,
        path = resultsFolder,
        fileName = "survival_results"
      )

      # Write results data model specification
      resultsDataModelSpecification <- self$getResultsDataModelSpecification()
      CohortGenerator::writeCsv(
        x = resultsDataModelSpecification,
        file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
        warnOnFileNameCaseMismatch = FALSE
      )

      # Disconnect from CDM
      CohortSurvival::cdmDisconnect(cdm)

      private$.message(paste("Results available at:", resultsFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      
      # Create cohort survival analysis results tables
      CohortSurvival::createSurvivalResultTables(
        connectionDetails = resultsConnectionDetails,
        targetDialect = resultsConnectionDetails$dbms,
        resultSchema = resultsDatabaseSchema,
        deleteTables = FALSE,
        createTables = TRUE,
        tablePrefix = tablePrefix
      )
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = self$tablePrefix) {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = system.file(
          file.path("csv", "survivalResultsDataModelSpecification.csv"),
          package = "CohortSurvival"
        ),
        warnOnCaseMismatch = FALSE
      )

      # Add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      
      # Find CSV files in the results folder
      csvFiles <- list.files(
        path = resultsFolder,
        pattern = "\\.csv$",
        full.names = TRUE
      )

      if (length(csvFiles) > 0) {
        # Upload each CSV file to the results database
        for (csvFile in csvFiles) {
          CohortSurvival::uploadSurvivalResults(
            connectionDetails = resultsConnectionDetails,
            schema = resultsDataModelSettings$resultsDatabaseSchema,
            csvFile = csvFile,
            tablePrefix = self$tablePrefix,
            purgeSiteDataBeforeUploading = FALSE
          )
        }
      }

      private$.message("Cohort survival analysis results uploaded successfully")
    },
    #' @description Creates the Cohort Survival Module Specifications
    #'
    #' @details
    #' Run cohort survival analyses for target cohorts and outcomes. This function supports both
    #' single event survival analysis and competing risk survival analysis.
    #'
    #' After completion, survival results can be plotted using the [CohortSurvival::plotSurvival()] function.
    #'
    #' @param analysisType The type of survival analysis to perform. Must be either "single_event" 
    #'                     or "competing_risk".
    #' @param targetCohortTable The name of the target cohort table.
    #' @param outcomeCohortTable The name of the outcome cohort table.
    #' @param competingOutcomeCohortTable The name of the competing outcome cohort table (required for competing risk analysis).
    #' @param strata A list of stratification variables. Each element should be a character vector of column names.
    #' @param timeGap The time gap for the analysis in days.
    #' @param followUp The follow-up period in days.
    #' @param minCellCount The minimum cell count for privacy protection.
    #'
    createModuleSpecifications = function(analysisType,
                                          targetCohortTable,
                                          outcomeCohortTable,
                                          competingOutcomeCohortTable = NULL,
                                          strata = NULL,
                                          timeGap = 7,
                                          followUp = 365,
                                          minCellCount = 5) {
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
    #' @param moduleSpecifications The Cohort Survival module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
      
      # Additional validation for cohort survival analysis specific requirements
      if (moduleSpecifications$settings$analysisType == "competing_risk" && 
          is.null(moduleSpecifications$settings$competingOutcomeCohortTable)) {
        stop("competingOutcomeCohortTable is required for competing risk analysis")
      }
    }
  )
)
