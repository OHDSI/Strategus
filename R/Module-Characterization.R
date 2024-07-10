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
    #' @param connectionDetails The connection details to the database
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param executionSettings The execution settings for the study
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      on.exit(private$.clearLoggers())
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
        characterizationSettings = jobContext$settings$analysis,
        databaseId = jobContext$moduleExecutionSettings$databaseId,
        saveDirectory = workFolder,
        tablePrefix = self$tablePrefix,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        incremental = jobContext$settings$incremental,
        threads = parallel::detectCores(),
        minCharacterizationMean = jobContext$settings$minCharacterizationMean
      )

      # move results from work folder to output folder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      csvFileLoc <- file.path(workFolder,'results')
      csvFiles <- dir(csvFileLoc)
      for(csvFile in csvFiles){
        message(paste0('Exporting csv file ', csvFile))
        file.copy(
          from = file.path(csvFileLoc, csvFile),
          to = file.path(resultsFolder, paste0(moduleInfo$TablePrefix,csvFile))
        )
      }

      # Export the resultsDataModelSpecification.csv
      resultsDataModel <- CohortGenerator::readCsv(
        file = system.file(
          "settings/resultsDataModelSpecification.csv",
          package = "Characterization"
        ),
        warnOnCaseMismatch = FALSE
      )

      # add the prefix to the tableName column
      resultsDataModel$tableName <- paste0(moduleInfo$TablePrefix, resultsDataModel$tableName)

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
    #' @param resultsConnectionDetails The connection details to the results database
    #' @param resultsSchema The schema holding the results
    #' @param tablePrefix The prefix to use to append to the results tables (optional)
    createResultsDataModel = function(resultsConnectionDetails, resultsSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsSchema, tablePrefix)
      Characterization::createCharacterizationTables(
        connectionDetails = resultsConnectionDetails,
        resultSchema = resultsSchema,
        targetDialect = connectionDetails$dbms,
        tablePrefix = tablePrefix
      )
    },
    #' @description Upload the results for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param resultsUploadSettings The results upload settings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsUploadSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsUploadSettings)
      # TODO: Needs implementation
      warning("NOT IMPLEMENTED")
    },
    #' @description Creates the CharacterizationModule Specifications
    #' @param targetIds A vector of cohort IDs to use as the target(s) for the characterization
    #' @param outcomeIds A vector of cohort IDs to use as the outcome(s) for the characterization
    #' @param outcomeWashoutDays The number of days between outcomes for washout
    #' @param minPriorObservation The minimum prior observation (in the target?)
    #' @param dechallengeStopInterval description
    #' @param dechallengeEvaluationWindow description
    #' @param riskWindowStart description
    #' @param startAnchor description
    #' @param riskWindowEnd description
    #' @param endAnchor description
    #' @param minCharacterizationMean description
    #' @param covariateSettings description
    #' @param caseCovariateSettings description
    #' @param casePreTargetDuration description
    #' @param casePostOutcomeDuration description
    #' @param incremental description
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
                                            longTermStartDays =  -365,
                                            shortTermStartDays = -30),
                                          caseCovariateSettings = Characterization::createDuringCovariateSettings(
                                            useConditionGroupEraDuring = T,
                                            useDrugGroupEraDuring = T,
                                            useProcedureOccurrenceDuring = T,
                                            useDeviceExposureDuring = T,
                                            useMeasurementDuring = T,
                                            useObservationDuring = T,
                                            useVisitConceptCountDuring = T),
                                          casePreTargetDuration = 365,
                                          casePostOutcomeDuration = 365,
                                          incremental = T) {
      # input checks

      if(!inherits(outcomeIds, "numeric")){
        stop("outcomeIdsList must be a numeric or a numeric vector")
      }

      if(!inherits(outcomeWashoutDays, "numeric")){
        stop("outcomeWashoutDays must be a numeric or a numeric vector")
      }
      if(length(outcomeIds) != length(outcomeWashoutDays)){
        stop("outcomeWashoutDaysVector and outcomeIdsList must be same length")
      }
      if(length(minPriorObservation) != 1){
        stop("minPriorObservation needs to be length 1")
      }
      if(length(riskWindowStart) != length(startAnchor) |
         length(riskWindowEnd) != length(startAnchor) |
         length(endAnchor) != length(startAnchor))
      {
        stop("Time-at-risk settings must be same length")
      }

      # group the outcomeIds with the same outcomeWashoutDays
      outcomeWashoutDaysVector <- unique(outcomeWashoutDays)
      outcomeIdsList <- lapply(
        outcomeWashoutDaysVector,
        function(x){
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

      for(j in 1:length(outcomeIdsList)){
        aggregateCovariateSettings[[length(aggregateCovariateSettings) + 1]] <- Characterization::createAggregateCovariateSettings(
          targetIds = targetIds,
          outcomeIds = outcomeIdsList[[j]],
          minPriorObservation = minPriorObservation,
          outcomeWashoutDays = outcomeWashoutDaysVector[j],
          riskWindowStart = riskWindowStart,
          startAnchor = startAnchor,
          riskWindowEnd = riskWindowEnd,
          endAnchor = endAnchor,
          covariateSettings = covariateSettings
        )
      }

      # TODO: This looks odd - why is analysis required to be nested?
      analysis <- list(
        analysis = Characterization::createCharacterizationSettings(
          timeToEventSettings = list(timeToEventSettings),
          dechallengeRechallengeSettings = list(dechallengeRechallengeSettings),
          aggregateCovariateSettings = aggregateCovariateSettings
        )
      )

      specifications <- super$createModuleSpecifications(
        moduleSpecifications = analysis
      )
      return(specifications)
    }
  )
)
