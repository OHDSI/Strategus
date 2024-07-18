# CharacterizationModule -------------
#' @title Module for generating cohort characterization information
#' @export
#' @description
#' Computes cohort characterization information against the OMOP CDM
#' NOTE: Using v1.0.3 version of module and
#' commit 372fb70c6133bdd8811f8dc1d2a2f9cb9a184345 for the
#' package
CharacterizationModule <- R6::R6Class(
  classname = "CharacterizationModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix to append to the results tables
    tablePrefix = "c_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Execute characterization
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      checkmate::assertClass(executionSettings, "CdmExecutionSettings")

      jobContext <- private$jobContext
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder

      Characterization::runCharacterizationAnalyses(
        connectionDetails = connectionDetails,
        targetDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        targetTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        characterizationSettings = jobContext$settings,
        databaseId = jobContext$moduleExecutionSettings$databaseId,
        saveDirectory = workFolder,
        tablePrefix = self$tablePrefix
      )

      # Export the results
      rlang::inform("Export data to csv files")

      sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = "sqlite",
        server = file.path(workFolder, "sqliteCharacterization", "sqlite.sqlite")
      )

      # get the result location folder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      Characterization::exportDatabaseToCsv(
        connectionDetails = sqliteConnectionDetails,
        resultSchema = "main",
        tempEmulationSchema = NULL,
        tablePrefix = self$tablePrefix,
        filePrefix = self$tablePrefix,
        saveDirectory = resultsFolder
      )

      # Export the resultsDataModelSpecification.csv
      resultsDataModel <- CohortGenerator::readCsv(
        file = system.file(
          "settings/resultsDataModelSpecification.csv",
          package = "Characterization"
        ),
        warnOnCaseMismatch = FALSE
      )

      # add the prefix to the tableName column
      resultsDataModel$tableName <- paste0(self$tablePrefix, resultsDataModel$tableName)

      CohortGenerator::writeCsv(
        x = resultsDataModel,
        file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
        warnOnCaseMismatch = FALSE,
        warnOnFileNameCaseMismatch = FALSE,
        warnOnUploadRuleViolations = FALSE
      )

      private$.message(paste("Results available at:", resultsFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      resultsDataModel <-private$.getResultsDataModelSpecification()
      sql <- ResultModelManager::generateSqlSchema(
        schemaDefinition = resultsDataModel
      )
      sql <- SqlRender::render(
        sql = sql,
        database_schema = resultsDatabaseSchema
      )
      connection <- DatabaseConnector::connect(
        connectionDetails = resultsConnectionDetails
      )
      on.exit(DatabaseConnector::disconnect(connection))
      DatabaseConnector::executeSql(
        connection = connection,
        sql = sql
      )
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)
      jobContext <- private$jobContext
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      specifications <- private$.getResultsDataModelSpecification()

      ResultModelManager::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        resultsFolder = resultsFolder,
        purgeSiteDataBeforeUploading = FALSE,
        specifications = specifications
      )
    },
    #' @description Creates the CharacterizationModule Specifications
    #' @param targetIds A vector of cohort IDs to use as the target(s) for the characterization
    #' @param outcomeIds A vector of cohort IDs to use as the outcome(s) for the characterization
    #' @param dechallengeStopInterval description
    #' @param dechallengeEvaluationWindow description
    #' @param timeAtRisk description
    #' @param minPriorObservation description
    #' @param minCharacterizationMean description
    #' @param covariateSettings description
    createModuleSpecifications = function(targetIds,
                                          outcomeIds,
                                          dechallengeStopInterval = 30,
                                          dechallengeEvaluationWindow = 30,
                                          timeAtRisk = data.frame(
                                            riskWindowStart = c(1, 1),
                                            startAnchor = c("cohort start", "cohort start"),
                                            riskWindowEnd = c(0, 365),
                                            endAnchor = c("cohort end", "cohort end")
                                          ),
                                          minPriorObservation = 0,
                                          minCharacterizationMean = 0,
                                          covariateSettings = FeatureExtraction::createDefaultCovariateSettings()) {
      # input checks
      if (!inherits(timeAtRisk, "data.frame")) {
        stop("timeAtRisk must be a data.frame")
      }
      if (nrow(timeAtRisk) == 0) {
        stop("timeAtRisk must be a non-empty data.frame")
      }

      timeToEventSettings <- Characterization::createTimeToEventSettings(
        targetIds = targetIds,
        outcomeIds = outcomeIds
      )

      dechallengeRechallengeSettings <- Characterization::createDechallengeRechallengeSettings(
        targetIds = targetIds,
        outcomeIds = outcomeIds,
        dechallengeStopInterval = dechallengeStopInterval,
        dechallengeEvaluationWindow = dechallengeEvaluationWindow
      )

      aggregateCovariateSettings <- lapply(
        X = 1:nrow(timeAtRisk),
        FUN = function(i) {
          Characterization::createAggregateCovariateSettings(
            targetIds = targetIds,
            outcomeIds = outcomeIds,
            minPriorObservation = minPriorObservation,
            riskWindowStart = timeAtRisk$riskWindowStart[i],
            startAnchor = timeAtRisk$startAnchor[i],
            riskWindowEnd = timeAtRisk$riskWindowEnd[i],
            endAnchor = timeAtRisk$endAnchor[i],
            covariateSettings = covariateSettings,
            minCharacterizationMean = minCharacterizationMean
          )
        }
      )

      analysis <- Characterization::createCharacterizationSettings(
        timeToEventSettings = list(timeToEventSettings),
        dechallengeRechallengeSettings = list(dechallengeRechallengeSettings),
        aggregateCovariateSettings = aggregateCovariateSettings
      )

      specifications <- super$createModuleSpecifications(
        moduleSpecifications = analysis
      )
      return(specifications)
    }
  ),
  private = list(
    .getResultsDataModelSpecification = function(tablePrefix = self$tablePrefix) {
      rdms <- CohortGenerator::readCsv(
        file = system.file(
          "settings/resultsDataModelSpecification.csv",
          package = "Characterization"
        )
      )
      rdms$tableName <- paste0(tablePrefix, rdms$tableName)
      return(rdms)
    }
  )
)
