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
    tablePrefix = "val_",
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

      jobContext <- private$jobContext
      cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext()
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      #library(PatientLevelPrediction)
      private$.message("Validating inputs")
      inherits(jobContext, 'list')

      if (is.null(jobContext$settings)) {
        stop("Analysis settings not found in job context")
      }
      if (is.null(jobContext$sharedResources)) {
        stop("Shared resources not found in job context")
      }
      if (is.null(jobContext$moduleExecutionSettings)) {
        stop("Execution settings not found in job context")
      }

      #workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      #resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      private$.message("Executing PLP Validation")
      #moduleInfo <- getModuleInfo()

      # find where cohortDefinitions are as sharedResources is a list
      # cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(
      #   sharedResources = jobContext$sharedResources,
      #   settings = jobContext$settings
      # )

      # check the model locations are valid and apply model
      upperWorkDir <- dirname(workFolder)
      modelTransferFolder <- sort(dir(upperWorkDir, pattern = 'ModelTransferModule'), decreasing = T)[1]

      modelSaveLocation <- file.path( upperWorkDir, modelTransferFolder, 'models') # hack to use work folder for model transfer
      modelInfo <- private$.getModelInfo(modelSaveLocation)

      designs <- list()
      for (i in seq_len(nrow(modelInfo))) {
        df <- modelInfo[i, ]

        design <- PatientLevelPrediction::createValidationDesign(
          targetId = df$target_id[1],
          outcomeId = df$outcome_id[1],
          plpModelList = as.list(df$modelPath),
          restrictPlpDataSettings = jobContext$settings[[1]]$restrictPlpDataSettings,
          populationSettings = jobContext$settings[[1]]$populationSettings
        )
        designs <- c(designs, design)
      }
      databaseNames <- c()
      databaseNames <- c(databaseNames, paste0(jobContext$moduleExecutionSettings$connectionDetailsReference))

      databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
        connectionDetails = jobContext$moduleExecutionSettings$connectionDetails,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cdmDatabaseName = jobContext$moduleExecutionSettings$connectionDetailsReference,
        cdmDatabaseId = jobContext$moduleExecutionSettings$databaseId,
        tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )

      PatientLevelPrediction::validateExternal(
        validationDesignList = designs,
        databaseDetails = databaseDetails,
        logSettings = PatientLevelPrediction::createLogSettings(verbosity = 'INFO', logName = 'validatePLP'),
        outputFolder = workFolder
      )

      sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = 'sqlite',
        server = file.path(workFolder, "sqlite", "databaseFile.sqlite")
      )

      PatientLevelPrediction::extractDatabaseToCsv(
        connectionDetails = sqliteConnectionDetails,
        databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
          resultSchema = 'main',
          tablePrefix = '',
          targetDialect = 'sqlite',
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
    #' @param validationComponentsList description
    createModuleSpecifications = function(validationComponentsList = list(
      list(
        targetId = 1,
        oucomeId = 4,
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), # vector
        validationSettings = PatientLevelPrediction::createValidationSettings(
          recalibrate = "weakRecalibration"
        ),
        populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
          riskWindowStart = 90,
          riskWindowEnd = 360,
          requireTimeAtRisk = F
        )
      ),
      list(
        targetId = 3,
        oucomeId = 4,
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), # vector
        validationSettings = PatientLevelPrediction::createValidationSettings(
          recalibrate = "weakRecalibration"
        )

      )
    )) {
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
    .getModelInfo = function(strategusOutputPath) {
      modelDesigns <- list.files(strategusOutputPath, pattern = "modelDesign.json",
                                 recursive = TRUE, full.names = TRUE)
      model <- NULL
      for (modelFilePath in modelDesigns) {
        directory <- dirname(modelFilePath)
        modelDesign <- ParallelLogger::loadSettingsFromJson(modelFilePath)

        if (is.null(model)) {
          model <- data.frame(
            target_id = modelDesign$targetId,
            outcome_id = modelDesign$outcomeId,
            modelPath = directory)
        } else {
          model <- rbind(model,
                         data.frame(
                           target_id = modelDesign$targetId,
                           outcome_id = modelDesign$outcomeId,
                           modelPath = directory))
        }
      }

      models <- model %>%
        dplyr::group_by(.data$target_id, .data$outcome_id) %>%
        dplyr::summarise(modelPath = list(.data$modelPath), .groups = "drop")
      return(models)
    },
    # this updates the cohort table details in covariates
    .updateCovariates <- function(plpModel, cohortTable, cohortDatabaseSchema){

      covSettings <- plpModel$modelDesign$covariateSettings
      # if a single setting make it into a list to force consistency
      if (inherits(covSettings, 'covariateSettings')) {
        covSettings <- list(covSettings)
      }

      for (i in 1:length(covSettings)) {
        if ('cohortTable' %in% names(covSettings[[i]])) {
          covSettings[[i]]$cohortTable <- cohortTable
        }
        if ('cohortDatabaseSchema' %in% names(covSettings[[i]])) {
          covSettings[[i]]$cohortDatabaseSchema <- cohortDatabaseSchema
        }
      }

      plpModel$modelDesign$covariateSettings <- covSettings

      return(plpModel)
    }
  )
)
