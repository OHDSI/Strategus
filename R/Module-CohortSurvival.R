# CohortSurvivalModule -------------
#' @title Kaplan-Meier survival analysis with the \href{https://github.com/darwin-eu/CohortSurvival}{CohortSurvival Package}
#' @export
#' @description
#' Module for performing Kaplan-Meier survival analysis in observational
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
      cdm <- CDMConnector::cdmFromCon(
        con = DatabaseConnector::connect(connectionDetails),
        cdmSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        writeSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTables = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )

      # Get settings from job context
      settings <- jobContext$settings

      # Run Kaplan-Meier survival analysis
      survivalResults <- CohortSurvival::estimateSingleEventSurvival(
        cdm = cdm,
        targetCohortTable = settings$targetCohortTable,
        outcomeCohortTable = settings$outcomeCohortTable,
        strata = settings$strata,
        eventGap = settings$timeGap,
        followUpDays = settings$followUp,
      )

      private$.message("Export data to csv files")

      # Export results to CSV
      CohortGenerator::writeCsv(
        x = survivalResults,
        file = file.path(resultsFolder, "survival_results.csv"),
        warnOnFileNameCaseMismatch = FALSE
      )

      # Write results data model specification
      resultsDataModelSpecification <- self$getResultsDataModelSpecification()
      CohortGenerator::writeCsv(
        x = resultsDataModelSpecification,
        file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
        warnOnFileNameCaseMismatch = FALSE
      )

      # Disconnect from CDM
      CDMConnector::cdmDisconnect(cdm)

      private$.message(paste("Results available at:", resultsFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)

      # Create Kaplan-Meier survival analysis results tables
      # Note: CohortSurvival doesn't have a createSurvivalResultTables function
      # Results are stored as CSV files and can be uploaded using standard methods
      private$.message("Survival results will be stored as CSV files")
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = self$tablePrefix) {
      # Create a simple results data model specification for survival results
      # Since CohortSurvival doesn't provide a predefined data model specification
      resultsDataModelSpecification <- data.frame(
        tableName = paste0(tablePrefix, "survival_results"),
        columnName = c(
          "cdm_name", "target_cohort", "outcome_name", "strata_name", "strata_level",
          "time", "n_risk", "n_event", "n_censor", "survival", "survival_se",
          "survival_lower", "survival_upper", "cumulative_failure", "cumulative_failure_se",
          "cumulative_failure_lower", "cumulative_failure_upper"
        ),
        dataType = c(
          "VARCHAR(255)", "VARCHAR(255)", "VARCHAR(255)", "VARCHAR(255)", "VARCHAR(255)",
          "INTEGER", "INTEGER", "INTEGER", "INTEGER", "FLOAT", "FLOAT", "FLOAT", "FLOAT",
          "FLOAT", "FLOAT", "FLOAT", "FLOAT"
        ),
        isRequired = c(rep("Yes", 17)),
        primaryKey = c("No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No", "No"),
        emptyIsNa = c(rep("Yes", 17))
      )
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
        # Upload each CSV file to the results database using standard methods
        for (csvFile in csvFiles) {
          # Read the CSV file
          data <- CohortGenerator::readCsv(csvFile, warnOnCaseMismatch = FALSE)

          # Upload to database using DatabaseConnector
          DatabaseConnector::insertTable(
            connection = DatabaseConnector::connect(resultsConnectionDetails),
            tableName = paste0(
              resultsDataModelSettings$resultsDatabaseSchema, ".",
              self$tablePrefix, "survival_results"
            ),
            data = data,
            dropTableIfExists = FALSE,
            createTable = TRUE,
            tempTable = FALSE
          )
        }
      }

      private$.message("Kaplan-Meier survival analysis results uploaded successfully")
    },
    #' @description Creates the Kaplan-Meier Survival Module Specifications
    #'
    #' @details
    #' Run Kaplan-Meier survival analyses for target cohorts and outcomes.
    #'
    #' After completion, survival results can be plotted using the [CohortSurvival::plotSurvival()] function.
    #'
    #' @param targetCohortTable The name of the target cohort table.
    #' @param outcomeCohortTable The name of the outcome cohort table.
    #' @param strata A list of stratification variables. Each element should be a character vector of column names.
    #' @param timeGap The time gap for the analysis in days.
    #' @param followUp The follow-up period in days.
    #' @param minCellCount The minimum cell count for privacy protection.
    #'
    createModuleSpecifications = function(targetCohortTable,
                                          outcomeCohortTable,
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
    #' @param moduleSpecifications The Kaplan-Meier Survival module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  )
)
