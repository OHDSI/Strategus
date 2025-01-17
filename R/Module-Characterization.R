# CharacterizationModule -------------
#' @title Characterize cohorts with the \href{https://ohdsi.github.io/Characterization/}{HADES Characterization Package}
#' @export
#' @description
#' Computes cohort characterization information against
#' the OMOP Common Data Model
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
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      # Handle cleaning of incremental folders/files
      if (isTRUE(executionSettings$incremental)) {
        Characterization::cleanIncremental(workFolder)
      } else {
        Characterization::cleanNonIncremental(workFolder)
      }

      Characterization::runCharacterizationAnalyses(
        connectionDetails = connectionDetails,
        targetDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        targetTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        characterizationSettings = jobContext$settings$analysis,
        databaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        outputDirectory = resultsFolder,
        executionPath = workFolder,
        csvFilePrefix = self$tablePrefix,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        minCharacterizationMean = jobContext$settings$minCharacterizationMean,
        incremental = jobContext$moduleExecutionSettings$incremental,
        threads = as.double(ifelse(Sys.getenv("CharacterizationThreads") == "", 1, Sys.getenv("CharacterizationThreads")))
      )

      # Export the resultsDataModelSpecification.csv
      resultsDataModelSpecification <- self$getResultsDataModelSpecification()

      CohortGenerator::writeCsv(
        x = resultsDataModelSpecification,
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

      Characterization::createCharacterizationTables(
        connectionDetails = resultsConnectionDetails,
        resultSchema = resultsDatabaseSchema,
        deleteExistingTables = T,
        createTables = T,
        tablePrefix = tablePrefix
      )
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = system.file(
          file.path("settings", "resultsDataModelSpecification.csv"),
          package = "Characterization"
        ),
        warnOnCaseMismatch = FALSE
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
    #' @param outcomeWashoutDays A vector of integers specifying the washout days for each outcome (same length as the outcomeIds)
    #' @param minPriorObservation The number of days of minimum observation a patient in the target populations must have
    #' @param dechallengeStopInterval description
    #' @param dechallengeEvaluationWindow description
    #' @param riskWindowStart The number of days after start anchor to start the time-at-risk (can be a vector for multiple TARS)
    #' @param startAnchor The TAR starts relative to this either cohort start or cohort end (can be a vector for multiple TARS)
    #' @param riskWindowEnd The number of days after end anchor to end the time-at-risk (can be a vector for multiple TARS)
    #' @param endAnchor The TAR ends relative to this either cohort start or cohort end (can be a vector for multiple TARS)
    #' @param minCharacterizationMean The minimum fraction patients in the target have a covariate for it to be included
    #' @param covariateSettings Covariates for the database, cohort and risk factor characterization
    #' @param caseCovariateSettings Covariates for the case-series characterization
    #' @param casePreTargetDuration The number of days before target start to use for case-series
    #' @param casePostOutcomeDuration The number of days after outcome start to use for case-series
    createModuleSpecifications = function(targetIds,
                                          outcomeIds, # a vector of ids
                                          outcomeWashoutDays = c(365), # same length as outcomeIds with the outcomeWashout
                                          minPriorObservation = 365,
                                          dechallengeStopInterval = 30,
                                          dechallengeEvaluationWindow = 30,
                                          riskWindowStart = c(1, 1),
                                          startAnchor = c("cohort start", "cohort start"),
                                          riskWindowEnd = c(0, 365),
                                          endAnchor = c("cohort end", "cohort end"),
                                          minCharacterizationMean = 0.01,
                                          covariateSettings = FeatureExtraction::createCovariateSettings(
                                            useDemographicsGender = T,
                                            useDemographicsAge = T,
                                            useDemographicsAgeGroup = T,
                                            useDemographicsRace = T,
                                            useDemographicsEthnicity = T,
                                            useDemographicsIndexYear = T,
                                            useDemographicsIndexMonth = T,
                                            useDemographicsTimeInCohort = T,
                                            useDemographicsPriorObservationTime = T,
                                            useDemographicsPostObservationTime = T,
                                            useConditionGroupEraLongTerm = T,
                                            useDrugGroupEraOverlapping = T,
                                            useDrugGroupEraLongTerm = T,
                                            useProcedureOccurrenceLongTerm = T,
                                            useMeasurementLongTerm = T,
                                            useObservationLongTerm = T,
                                            useDeviceExposureLongTerm = T,
                                            useVisitConceptCountLongTerm = T,
                                            useConditionGroupEraShortTerm = T,
                                            useDrugGroupEraShortTerm = T,
                                            useProcedureOccurrenceShortTerm = T,
                                            useMeasurementShortTerm = T,
                                            useObservationShortTerm = T,
                                            useDeviceExposureShortTerm = T,
                                            useVisitConceptCountShortTerm = T,
                                            endDays = 0,
                                            longTermStartDays = -365,
                                            shortTermStartDays = -30
                                          ),
                                          caseCovariateSettings = Characterization::createDuringCovariateSettings(
                                            useConditionGroupEraDuring = T,
                                            useDrugGroupEraDuring = T,
                                            useProcedureOccurrenceDuring = T,
                                            useDeviceExposureDuring = T,
                                            useMeasurementDuring = T,
                                            useObservationDuring = T,
                                            useVisitConceptCountDuring = T
                                          ),
                                          casePreTargetDuration = 365,
                                          casePostOutcomeDuration = 365) {
      # input checks
      if (!inherits(outcomeIds, "numeric")) {
        stop("outcomeIds must be a numeric or a numeric vector")
      }

      if (!inherits(outcomeWashoutDays, "numeric")) {
        stop("outcomeWashoutDays must be a numeric or a numeric vector")
      }
      if (length(outcomeIds) != length(outcomeWashoutDays)) {
        stop("outcomeWashoutDaysVector and outcomeIds must be same length")
      }
      if (length(minPriorObservation) != 1) {
        stop("minPriorObservation needs to be length 1")
      }
      if (length(riskWindowStart) != length(startAnchor) |
        length(riskWindowEnd) != length(startAnchor) |
        length(endAnchor) != length(startAnchor)) {
        stop("Time-at-risk settings must be same length")
      }

      # group the outcomeIds with the same outcomeWashoutDays
      outcomeWashoutDaysVector <- unique(outcomeWashoutDays)
      outcomeIdsList <- lapply(
        outcomeWashoutDaysVector,
        function(x) {
          ind <- which(outcomeWashoutDays == x)
          unique(outcomeIds[ind])
        }
      )


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

      aggregateCovariateSettings <- list()

      for (i in 1:length(riskWindowStart)) {
        for (j in 1:length(outcomeIdsList)) {
          aggregateCovariateSettings[[length(aggregateCovariateSettings) + 1]] <- Characterization::createAggregateCovariateSettings(
            targetIds = targetIds,
            outcomeIds = outcomeIdsList[[j]],
            minPriorObservation = minPriorObservation,
            outcomeWashoutDays = outcomeWashoutDaysVector[j],
            riskWindowStart = riskWindowStart[i],
            startAnchor = startAnchor[i],
            riskWindowEnd = riskWindowEnd[i],
            endAnchor = endAnchor[i],
            covariateSettings = covariateSettings,
            caseCovariateSettings = caseCovariateSettings,
            casePreTargetDuration = casePreTargetDuration,
            casePostOutcomeDuration = casePostOutcomeDuration
          )
        }
      }

      analysis <- Characterization::createCharacterizationSettings(
        timeToEventSettings = list(timeToEventSettings),
        dechallengeRechallengeSettings = list(dechallengeRechallengeSettings),
        aggregateCovariateSettings = aggregateCovariateSettings
      )

      specifications <- super$createModuleSpecifications(
        moduleSpecifications = list(
          analysis = analysis,
          minCharacterizationMean = minCharacterizationMean
        )
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
