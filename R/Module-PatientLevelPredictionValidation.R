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


      groupedModelInfo <- modelInfo %>%
        filter(!(model_type %in% c("ResNet", "Transformer"))) %>%
        group_by(target_id, outcome_id)

      splitModelInfo <- split(groupedModelInfo, list(groupedModelInfo$target_id, groupedModelInfo$outcome_id), drop = TRUE)

      designs <- list()
      for (i in seq_along(splitModelInfo)) {
        df <- splitModelInfo[[i]]

        design <- PatientLevelPrediction::createValidationDesign(
          targetId = df$target_id[1],
          outcomeId = df$outcome_id[1],
          populationSettings = NULL, # use setting from model
          restrictPlpDataSettings = NULL, # use setting from model
          plpModelList = as.list(df$plp_model_file)
        )
        designs[[i]] <- design  # Adding elements to a list
      }

      databaseDetails <- list()
      databaseNames <- c()
      databaseNames <- c(databaseNames, paste0(jobContext$moduleExecutionSettings$cdmDatabaseMetaData$cdmSourceAbbreviation))

      databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
        connectionDetails = jobContext$moduleExecutionSettings$connectionDetails,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cdmDatabaseName = paste0(jobContext$moduleExecutionSettings$connectionDetailsReference),
        cdmDatabaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        #tempEmulationSchema =  , is there s temp schema specified anywhere?
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
          resultSchema = 'main', # sqlite settings
          tablePrefix = '', # sqlite settings
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

      combinedData <- NULL
      subdirs <- list.files(strategusOutputPath, full.names = TRUE)

      for (dir in subdirs) {
        folder <- basename(dir)
        print(basename(folder))

        allFiles <- list.files(dir, pattern = "models.csv", full.names = TRUE, recursive = TRUE)

        for(modelFilePath in allFiles) {
          directoryPath <- dirname(modelFilePath)
          databaseDetailsPath <- file.path(directoryPath, "database_details.csv")
          databaseMetaDataPath <- file.path(directoryPath, "database_meta_data.csv")
          modelDesign <- file.path(directoryPath, "model_designs.csv")
          cohorts <- file.path(directoryPath, "cohorts.csv")
          populationSettingsPath <- file.path(directoryPath, "population_settings.csv")
          plpDataSettingsPath <- file.path(directoryPath, "plp_data_settings.csv")

          modelData <- readr::read_csv(modelFilePath, show_col_types=FALSE)
          databaseDetails <- readr::read_csv(databaseDetailsPath, show_col_types=FALSE)
          databaseMetaData <- readr::read_csv(databaseMetaDataPath, show_col_types=FALSE)
          modelDesign <- readr::read_csv(modelDesign, show_col_types=FALSE)
          cohorts <- readr::read_csv(cohorts, show_col_types=FALSE)
          populationSettings <- readr::read_csv(populationSettingsPath, show_col_types=FALSE)
          plpDataSettings <- readr::read_csv(plpDataSettingsPath, show_col_types=FALSE)

          modelData$plp_model_file <- file.path(directoryPath, "models", basename(modelData$plp_model_file))

          enrichedData <- merge(modelData, databaseDetails, by = "database_id")
          finalModelData <- merge(enrichedData, databaseMetaData, by.y = "database_id", by.x = "database_meta_data_id")
          finalModelData <- merge(finalModelData, modelDesign, by = "model_design_id")
          finalModelData <- merge(finalModelData, populationSettings, by = "population_setting_id")
          finalModelData <- merge(finalModelData, plpDataSettings, by = "plp_data_setting_id")
          finalModelData <- merge(finalModelData, cohorts, by.x = "outcome_id", by.y = "cohort_id")
          finalModelData <- within(finalModelData, {
            outcome_id <- cohort_definition_id
          })
          finalModelData$cohort_definition_id <- NULL

          finalModelData <- merge(finalModelData, cohorts, by.x = "target_id", by.y = "cohort_id")
          finalModelData <- within(finalModelData, {
            target_id <- cohort_definition_id
          })

          if(is.null(combinedData)) {
            combinedData <- finalModelData
          } else {
            combinedData <- rbind(combinedData, finalModelData)
          }
        }
      }
      finalSelectedData <- combinedData %>%
        select(cdm_source_abbreviation, analysis_id, model_design_id, model_type, target_id, outcome_id, plp_model_file,
               plp_data_settings_json, population_settings_json)
    }
    # AGS: I don't see .predictGLM or .updateCovariates used by the module?
    # .predictGLM <- function(plpModel, data, cohort){
    #   #require('dplyr')
    #   start <- Sys.time()
    #
    #   ParallelLogger::logTrace('predictProbabilities using predictGLM')
    #
    #   data$covariateData$coefficients <- plpModel$model$coefficients
    #   on.exit(data$covariateData$coefficients <- NULL)
    #
    #   prediction <- data$covariateData$covariates %>%
    #     dplyr::inner_join(data$covariateData$coefficients, by= 'covariateId') %>%
    #     dplyr::mutate(values = .data$covariateValue*.data$coefficient) %>%
    #     dplyr::group_by(.data$rowId) %>%
    #     dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
    #     dplyr::select("rowId", "value")
    #
    #   prediction <- as.data.frame(prediction)
    #   prediction <- merge(cohort, prediction, by ="rowId", all.x = TRUE, fill = 0)
    #   prediction$value[is.na(prediction$value)] <- 0
    #   prediction$value <- prediction$value + plpModel$model$intercept
    #
    #   # linear/logistic/square/exponential
    #   if(plpModel$model$finalMapping == 'linear'){
    #     prediction$value <- prediction$value
    #   } else if(plpModel$model$finalMapping == 'logistic'){
    #     prediction$value <- 1/(1+exp(-prediction$value))
    #   } else if(plpModel$model$finalMapping == 'square'){
    #     prediction$value <- prediction$value^2
    #   } else if(plpModel$model$finalMapping == 'exponential'){
    #     prediction$value <- exp(prediction$value)
    #   }
    #
    #   attr(prediction, "metaData")$modelType <- "binary"
    #
    #   delta <- Sys.time() - start
    #   ParallelLogger::logInfo("Prediction took ", signif(delta, 3), " ", attr(delta, "units"))
    #   return(prediction)
    # },
    # # this updates the cohort table details in covariates
    # .updateCovariates <- function(plpModel, cohortTable, cohortDatabaseSchema){
    #
    #   covSettings <- plpModel$modelDesign$covariateSettings
    #   # if a single setting make it into a list to force consistency
    #   if(inherits(covSettings, 'covariateSettings')){
    #     covSettings <- list(covSettings)
    #   }
    #
    #   for(i in 1:length(covSettings)){
    #     if('cohortTable' %in% names(covSettings[[i]])){
    #       covSettings[[i]]$cohortTable <- cohortTable
    #     }
    #     if('cohortDatabaseSchema' %in% names(covSettings[[i]])){
    #       covSettings[[i]]$cohortDatabaseSchema <- cohortDatabaseSchema
    #     }
    #   }
    #
    #   plpModel$modelDesign$covariateSettings <- covSettings
    #
    #   return(plpModel)
    # }
  )
)
