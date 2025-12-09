library(omopgenerics)
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
    tablePrefix = "cs_",
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

      # get a DBI Connection object - Cohort Survival works with this only
      dbi_conn <- DatabaseConnector::connect(connectionDetails)@dbiConnection

      # Get settings from job context
      settings <- jobContext$settings
      # ---- Handle strata ----
      strata_param <- NULL
      strata_cols <- list()
      if (!is.null(settings$strata)) {
        cohort_cols <- DBI::dbListFields(dbi_conn, jobContext$moduleExecutionSettings$cohortTableNames$cohortTable)
        for (strata_name in settings$strata) {
          sanitized_name <- tolower(strata_name)
          sanitized_name <- gsub("[^[:alnum:][:space:]]", "", sanitized_name)
          sanitized_name <- gsub("\\s+", "_", sanitized_name)
          column_name <- paste0("strata_", sanitized_name)

          if (!(column_name %in% cohort_cols)) {
            if (strata_name == "gender") {
              # Add gender strata as text
              DBI::dbExecute(dbi_conn, paste0(
                "ALTER TABLE ", jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, " ADD COLUMN ", column_name, " TEXT;"
              ))
              DBI::dbExecute(dbi_conn, paste0(
                "UPDATE ", jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, " AS c ",
                "SET ", column_name, " = CASE ",
                "WHEN p.gender_concept_id = 8507 THEN 'male' ",
                "WHEN p.gender_concept_id = 8532 THEN 'female' ",
                "ELSE 'unknown' END ",
                "FROM person p WHERE c.subject_id = p.person_id;"
              ))
            } else if (strata_name == "age") {
              # Add age group strata as text
              DBI::dbExecute(dbi_conn, paste0(
                "ALTER TABLE ", jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, " ADD COLUMN ", column_name, " TEXT;"
              ))
              current_year <- as.numeric(format(Sys.Date(), "%Y"))
              DBI::dbExecute(dbi_conn, paste0(
              "UPDATE ", jobContext$moduleExecutionSettings$cohortTableNames$cohortTable, " AS c ",
              "SET ", column_name, " = CASE ",
              "WHEN (", current_year, " - p.year_of_birth) < 18 THEN '0-17' ",
              "WHEN (", current_year, " - p.year_of_birth) BETWEEN 18 AND 34 THEN '18-34' ",
              "WHEN (", current_year, " - p.year_of_birth) BETWEEN 35 AND 49 THEN '35-49' ",
              "WHEN (", current_year, " - p.year_of_birth) BETWEEN 50 AND 64 THEN '50-64' ",
              "ELSE '65+' END ",
              "FROM person p WHERE c.subject_id = p.person_id;"
            ))
            }
          }
        }

        # Pass all strata columns to survival function
        cohort_cols <- DBI::dbListFields(dbi_conn, jobContext$moduleExecutionSettings$cohortTableNames$cohortTable)
        strata_cols <- cohort_cols[grepl("^strata_", cohort_cols)]
        if (length(strata_cols) > 0) {
          strata_param <- lapply(strata_cols, function(col) c(col))
        }
      }
      
      # Create CDM object for CohortSurvival
      cdm <- CDMConnector::cdmFromCon(
        con = dbi_conn,
        cdmSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        writeSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTables = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )
      if (settings$analysisType == "single_event") {
        # Run Kaplan-Meier survival analysis
        survivalResults <- CohortSurvival::estimateSingleEventSurvival(
          cdm = cdm,
          targetCohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
          targetCohortId = settings$targetCohortId,
          outcomeCohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
          outcomeCohortId = settings$outcomeCohortId,
          strata = strata_param,
          eventGap = settings$eventGap,
          followUpDays = settings$followUpDays
        )
      } else if (settings$analysisType == "competing_risk") {
        # Competing risk cohort survival analysis
        survivalResults <- CohortSurvival::estimateCompetingRiskSurvival(
          cdm = cdm,
          targetCohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
          targetCohortId = settings$targetCohortId,
          outcomeCohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
          outcomeCohortId = settings$outcomeCohortId,
          competingOutcomeCohortTable = settings$competingOutcomeCohortTable,
          eventGap = settings$eventGap,
          followUpDays = settings$followUpDays
        )
      } else {
        stop("Invalid analysis type. Must be 'single_event' or 'competing_risk'")
      }
      private$.message("Export data to csv files")
      # Export results to CSV
      omopgenerics::exportSummarisedResult(survivalResults, fileName = file.path(resultsFolder, "survival_results.csv"))
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

      CohortMethod::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        zipFileName = zipFileName,
        purgeSiteDataBeforeUploading = FALSE
      )
      private$.message("Cohort survival analysis results uploaded successfully")
    },
    #' @description Creates the Kaplan-Meier Survival Module Specifications
    #'
    #' @details
    #' create module specifications for survival analysis of single event as well as competing risk
    #' @param strata A list of stratification variables. Each element should be a character vector of column names.
    #' @param eventGap The time gap for the analysis in days.
    #' @param followUpDays The follow-up period in days.
    #'
    createModuleSpecifications = function(targetCohortId,
                                          outcomeCohortId,
                                          strata = NULL,
                                          eventGap = 7,
                                          followUpDays = 365,
                                          competingOutcomeCohortTable = NULL,
                                          analysisType = "single_event") {
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
