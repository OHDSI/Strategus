# PatientLevelPredictionModule -------------
#' @title Patient-level prediction with the \href{https://ohdsi.github.io/PatientLevelPrediction/}{HADES PatientLevelPrediction Package}
#' @export
#' @description
#' Module for performing patient-level prediction in an observational
#' database in the OMOP Common Data Model.
PatientLevelPredictionModule <- R6::R6Class(
  classname = "PatientLevelPredictionModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix to append to the results tables
    tablePrefix = "plp_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the PatientLevelPrediction package
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

      # Creating database details
      databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cdmDatabaseName = jobContext$moduleExecutionSettings$connectionDetailsReference,
        cdmDatabaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )

      jobContext$settings$modelDesignList <- private$.setCovariateSchemaTable(
        modelDesignList = jobContext$settings$modelDesignList,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )

      # run the models
      PatientLevelPrediction::runMultiplePlp(
        databaseDetails = databaseDetails,
        modelDesignList = jobContext$settings$modelDesignList,
        cohortDefinitions = cohortDefinitionSet,
        saveDirectory = workFolder,
        skipDiagnostics = jobContext$settings$skipDiagnostics
      )

      private$.message("Export data to csv files")

      sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = "sqlite",
        server = file.path(workFolder, "sqlite", "databaseFile.sqlite")
      )

      PatientLevelPrediction::extractDatabaseToCsv(
        connectionDetails = sqliteConnectionDetails,
        databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
          resultSchema = "main", # sqlite settings
          tablePrefix = "", # sqlite settings
          targetDialect = "sqlite",
          tempEmulationSchema = NULL
        ),
        csvFolder = file.path(resultsFolder),
        fileAppend = NULL
      )

      resultsDataModel <- self$getResultsDataModelSpecification()
      CohortGenerator::writeCsv(
        x = resultsDataModel,
        file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
        warnOnFileNameCaseMismatch = F
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
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = private$.getResultsDataModelSpecificationFileLocation()
      )

      # add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, self$tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
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
    #' @description Creates the PatientLevelPrediction Module Specifications
    #' @param modelDesignList A list of model designs created using \code{PatientLevelPrediction::createModelDesign()}
    #' @param skipDiagnostics Whether to run the diagnostics based on PROBAST before model development
    createModuleSpecifications = function(modelDesignList, skipDiagnostics = FALSE) {
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
    #' @param moduleSpecifications The PatientLevelPrediction module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  ),
  private = list(
    .setCovariateSchemaTable = function(modelDesignList,
                                        cohortDatabaseSchema,
                                        cohortTable) {
      if (inherits(modelDesignList, "modelDesign")) {
        modelDesignList <- list(modelDesignList)
      }

      for (i in 1:length(modelDesignList)) {
        covariateSettings <- modelDesignList[[i]]$covariateSettings

        if (inherits(covariateSettings, "covariateSettings")) {
          covariateSettings <- list(covariateSettings)
        }

        for (j in 1:length(covariateSettings)) {
          if ("cohortDatabaseSchema" %in% names(covariateSettings[[j]])) {
            covariateSettings[[j]]$cohortDatabaseSchema <- cohortDatabaseSchema
          }
          if ("cohortTable" %in% names(covariateSettings[[j]])) {
            covariateSettings[[j]]$cohortTable <- cohortTable
          }
        }

        modelDesignList[[i]]$covariateSettings <- covariateSettings
      }

      return(modelDesignList)
    },
    .getResultsDataModelSpecificationFileLocation = function() {
      return(system.file(
        file.path("settings", "resultsDataModelSpecification.csv"),
        package = "PatientLevelPrediction"
      ))
    }
  )
)
