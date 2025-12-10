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
      private$.message("Exporting data to csv files")
      # Export results to CSV
      omopgenerics::exportSummarisedResult(survivalResults, fileName = file.path(resultsFolder, "survival_results.csv"))
      # Disconnect from CDM
      CDMConnector::cdmDisconnect(cdm)
      private$.message("Successfully exported data to csv files")

      # private$.message("Creating results data model specification")
      # resultsDataModelSpecification <- self$getResultsDataModelSpecification()
      # CohortGenerator::writeCsv(
      #   x = resultsDataModelSpecification,
      #   file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
      #   warnOnFileNameCaseMismatch = FALSE
      # )
      # private$.message("Successfully created results data model specification")
      
      private$.message(paste("Results available at:", resultsFolder))
    },

    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModel = function(tablePrefix = "cs_") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = system.file(
          file.path("csv", "resultsDataModelSpecification.csv"),
          package = "CohortSurvival"
        ),
        warnOnCaseMismatch = FALSE
      )

      # add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },
    
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)

      if (connectionDetails$dbms == "sqlite" & databaseSchema != "main") {
        stop("Invalid schema for sqlite, use databaseSchema = 'main'")
      }
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))
      # Create first version of results model:
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "CreateResultsDataModel.sql",
        packageName = "CohortSurvival",
        dbms = connection@dbms,
        database_schema = databaseSchema,
        table_prefix = tablePrefix
      )
      DatabaseConnector::executeSql(connection, sql)
      # Migrate to current version:
      # migrateDataModel(
      #   connectionDetails = connectionDetails,
      #   databaseSchema = databaseSchema,
      #   tablePrefix = tablePrefix
      # )
      private$.message("Results data model created successfully")
    },
    
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      ResultModelManager::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        resultsFolder = private$jobContext$moduleExecutionSettings$resultsSubFolder,
        tablePrefix = resultsDataModelSettings$tablePrefix,
        forceOverWriteOfSpecifications = FALSE,
        purgeSiteDataBeforeUploading = TRUE,
        runCheckAndFixCommands = FALSE,
        specifications = self$getResultsDataModelSpecifications(resultsDataModelSettings$tablePrefix),
        warnOnMissingTable = FALSE,
        ...
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
