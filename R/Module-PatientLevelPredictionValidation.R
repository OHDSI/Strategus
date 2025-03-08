# PatientLevelPredictionValidationModule -------------
#' @title Module for performing validation of patient-level prediction models
#' @export
#' @description
#' Module for performing patient-level prediction model validation for models
#' built using the PatientLevelPrediction package.
PatientLevelPredictionValidationModule <- R6::R6Class(
  classname = "PatientLevelPredictionValidationModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix to append to the results tables
    tablePrefix = "plp_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the PatientLevelPrediction package to validate a
    #' PLP model
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      private$.message("Executing PLP Validation")

      jobContext <- private$jobContext
      # cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext()
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      # library(PatientLevelPrediction)
      private$.message("Validating inputs")
      inherits(jobContext, "list")

      if (is.null(jobContext$settings)) {
        stop("Analysis settings not found in job context")
      }
      # if (is.null(jobContext$sharedResources)) {
      #   stop("Shared resources not found in job context")
      # }
      if (is.null(jobContext$moduleExecutionSettings)) {
        stop("Execution settings not found in job context")
      }

      # hack to eval modelList strings using package name
      # this converts system.file('model_folder', package = 'study') to the path
      private$.message("Updating modelDesigns in validationlist")
      jobContext$settings$validationList <- private$.updatePlpModelList(validationList = jobContext$settings$validationList)

      # update covariate settings schema and table to use cohort generator settings
      private$.message("Updating schema and table name for cohort covariates")
      jobContext$settings$validationList <- private$.setCovariateSchemaTable(
        validationList = jobContext$settings$validationList,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )

      private$.message("Setting databaseDetails")
      databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cdmDatabaseName = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$cdmSourceAbbreviation,
        cdmDatabaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )

      private$.message("Creating cohortDefinitionSet")
      cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext()

      private$.message("Running validateExternal in PatientLevelPrediction")
      # TODO Add connectionDetails into this after PLP is updated?
      PatientLevelPrediction::validateExternal(
        validationDesignList = jobContext$settings$validationList,
        databaseDetails = databaseDetails,
        logSettings = PatientLevelPrediction::createLogSettings(verbosity = jobContext$settings$logLevel, logName = "validatePLP"),
        outputFolder = workFolder
        # ,cohortDefinitions = cohortDefinitionSet
      )

      private$.message("Exporting results to csv")
      sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = "sqlite",
        server = file.path(workFolder, "sqlite", "databaseFile.sqlite")
      )

      PatientLevelPrediction::extractDatabaseToCsv(
        connectionDetails = sqliteConnectionDetails,
        databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
          resultSchema = "main",
          tablePrefix = "",
          targetDialect = "sqlite",
          tempEmulationSchema = NULL
        ),
        csvFolder = resultsFolder,
        fileAppend = NULL
      )

      private$.message(paste("Results available at:", resultsFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      PatientLevelPrediction::createPlpResultTables(
        connectionDetails = resultsConnectionDetails,
        targetDialect = resultsConnectionDetails$dbms,
        resultSchema = resultsDatabaseSchema,
        deleteTables = F,
        createTables = T,
        tablePrefix = tablePrefix
      )
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      databaseSchemaSettings <- PatientLevelPrediction::createDatabaseSchemaSettings(
        resultSchema = resultsDataModelSettings$resultsDatabaseSchema,
        tablePrefix = self$tablePrefix,
        targetDialect = resultsConnectionDetails$dbms
      )

      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      # TODO: This function does not expose
      # a way to specify the database identifier file
      # which makes the purge problematic since I'm
      # not sure how it will know what to purge...
      PatientLevelPrediction::insertCsvToDatabase(
        csvFolder = resultsFolder,
        connectionDetails = resultsConnectionDetails,
        databaseSchemaSettings = databaseSchemaSettings,
        modelSaveLocation = file.path(resultsFolder, "dbmodels"),
        csvTableAppend = ""
      )
    },
    #' @description Creates the PatientLevelPredictionValidation Module Specifications
    #' @param validationList A list of validation designs from `PatientLevelPrediction::createValidationDesign`
    #' @param logLevel The logging level while executing the model validation.
    createModuleSpecifications = function(
        validationList = list(
          PatientLevelPrediction::createValidationDesign(
            plpModelList = list(file.path("location_to_model")),
            targetId = 1,
            outcomeId = 3,
            restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
            populationSettings = NULL,
            recalibrate = "weakRecalibration",
            runCovariateSummary = TRUE
          ),
          PatientLevelPrediction::createValidationDesign(
            plpModelList = list(file.path("location_to_model")),
            targetId = 4,
            outcomeId = 3,
            restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
            populationSettings = NULL,
            recalibrate = "weakRecalibration",
            runCovariateSummary = TRUE
          )
        ),
        logLevel = "INFO") {
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
    #' @param moduleSpecifications The PatientLevelPredictionValidation module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  ),
  private = list(
    .setCovariateSchemaTable = function(validationList,
                                        cohortDatabaseSchema,
                                        cohortTable) {
      if (inherits(validationList, "validationDesign")) {
        validationList <- list(validationList)
      }

      for (i in 1:length(validationList)) {
        if (inherits(validationList[[i]]$plpModelList, "plpModel")) {
          validationList[[i]]$plpModelList <- list(validationList[[i]]$plpModelList)
        }

        for (j in 1:length(validationList[[i]]$plpModelList)) {
          covariateSettings <- validationList[[i]]$plpModelList[[j]]$modelDesign$covariateSettings

          if (inherits(covariateSettings, "covariateSettings")) {
            covariateSettings <- list(covariateSettings)
          }

          for (k in 1:length(covariateSettings)) {
            if ("cohortDatabaseSchema" %in% names(covariateSettings[[k]])) {
              covariateSettings[[k]]$cohortDatabaseSchema <- cohortDatabaseSchema
            }
            if ("cohortTable" %in% names(covariateSettings[[k]])) {
              covariateSettings[[k]]$cohortTable <- cohortTable
            }
          }

          validationList[[i]]$plpModelList[[j]]$modelDesign$covariateSettings <- covariateSettings
        }
      }

      return(validationList)
    },
    .updatePlpModelList = function(validationList) {
      if (inherits(validationList, "validationDesign")) {
        validationList <- list(validationList)
      }

      for (i in 1:length(validationList)) {
        if (inherits(validationList[[i]]$plpModelList, "plpModel")) {
          validationList[[i]]$plpModelList <- list(validationList[[i]]$plpModelList)
        }

        for (j in 1:length(validationList[[i]]$plpModelList)) {
          # code to convert the package type to a file path for the model
          # in the package
          if (!is.null(validationList[[i]]$plpModelList[[j]]$type)) {
            if (validationList[[i]]$plpModelList[[j]]$type == "package") {
              validationList[[i]]$plpModelList[[j]] <- PatientLevelPrediction::loadPlpModel(system.file(
                validationList[[i]]$plpModelList[[j]]$modelFolder,
                package = validationList[[i]]$plpModelList[[j]]$package
              ))
            }
          }

          # add other type changes here
        }
      }

      return(validationList)
    }
  )
)
