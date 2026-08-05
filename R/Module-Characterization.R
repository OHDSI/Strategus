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
      # 1) Always clean the incremental files
      Characterization::cleanIncremental(
        executionFolder = workFolder,
        ignoreWhenEmpty = TRUE
      )
      # 2) If we're running in non-incremental mode,
      #    make sure the work folder is completely cleaned
      if (isFALSE(executionSettings$incremental)) {
        Characterization::cleanNonIncremental(workFolder)
      }

      Characterization::runCharacterizationAnalyses(
        connectionDetails = connectionDetails,
        targetDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        targetTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        nestingCohortDatabaseSchema  = jobContext$moduleExecutionSettings$workDatabaseSchema,
        nestingCohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        outputDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        outputTable = jobContext$settings$outputTable,
        tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        characterizationSettings = jobContext$settings$analysis,
        outputDirectory = resultsFolder,
        executionPath = workFolder,
        csvFilePrefix = self$tablePrefix,
        databaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        incremental = jobContext$moduleExecutionSettings$incremental,
        threads = as.double(ifelse(Sys.getenv("CharacterizationThreads") == "", 1, Sys.getenv("CharacterizationThreads"))),
        cohortGenerationThreads = as.double(ifelse(Sys.getenv("CharacterizationCohortGenerationThreads") == "", 1, Sys.getenv("CharacterizationCohortGenerationThreads"))),
        nTargetJobs = as.double(ifelse(Sys.getenv("CharacterizationNTargetJobs") == "", 1, Sys.getenv("CharacterizationNTargetJobs"))),
        minCharacterizationMean = jobContext$settings$minCharacterizationMean,
        minCovariateCount = jobContext$settings$minCovariateCount,
        mode = jobContext$settings$mode,
        minSMD = jobContext$settings$minSMD,
        minTargetSize = jobContext$settings$minTargetSize,
        minCaseSize = jobContext$settings$minCaseSize
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
        deleteExistingTables = TRUE,
        createTables = TRUE,
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
    #' @param characterizationSettings The settings defined using Characterization::createCharacterizationSettings
    #' @param targetIds Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param limitToFirstInNDays Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param minPriorObservation Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param outcomeIds Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param outcomeWashoutDays Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param riskWindowStart Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param startAnchor Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param riskWindowEnd Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param endAnchor Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param dechallengeStopInterval Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param dechallengeEvaluationWindow Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param covariateSettings Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param caseCovariateSettings Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param casePreTargetDuration Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param casePostOutcomeDuration Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param includeTimeToEvent Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param includeDechallengeRechallenge Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param includeTargetBaseline Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param includeRiskFactors Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param includeCaseSeries Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param mode Pick one of 'CohortIncidence'/'Efficient'/'PatientLevelPrediction' to specify how the non-cases are defined
    #' @param minSMD The minimum standardized mean difference for the risk factors analysis
    #' @param minCharacterizationMean The minimum fraction patients in the target have a covariate for it to be included
    #' @param minCovariateCount The minimum number of patients in the analysis to have a covariate for it to be included
    #' @param minTargetSize Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param minCaseSize Deprecated. Use Characterization v4 settings objects via `characterizationSettings`.
    #' @param outputTable The table used by characterization to create the cohorts used in characterization (included targets, cases, non-cases, etc.). The spec hash and database hash are added to this name to make it unique per run.
    createModuleSpecifications = function(
      characterizationSettings = NULL,
      targetIds = NULL,
      limitToFirstInNDays = NULL,
      minPriorObservation = NULL,
      outcomeIds = NULL,
      outcomeWashoutDays = NULL,
      riskWindowStart = NULL,
      startAnchor = NULL,
      riskWindowEnd = NULL,
      endAnchor = NULL,
      dechallengeStopInterval = NULL,
      dechallengeEvaluationWindow = NULL,
      covariateSettings = NULL,
      caseCovariateSettings = NULL,
      casePreTargetDuration = NULL,
      casePostOutcomeDuration = NULL,
      includeTimeToEvent = NULL,
      includeDechallengeRechallenge = NULL,
      includeTargetBaseline = NULL,
      includeRiskFactors = NULL,
      includeCaseSeries = NULL,
      mode = "CohortIncidence",
      minSMD = 0,
      minCharacterizationMean = 0,
      minCovariateCount = 0,
      minTargetSize = 0,
      minCaseSize = 0,
      outputTable = "characterization_cohorts"
      ) {

      legacyArguments <- list(
        targetIds = targetIds,
        limitToFirstInNDays = limitToFirstInNDays,
        minPriorObservation = minPriorObservation,
        outcomeIds = outcomeIds,
        outcomeWashoutDays = outcomeWashoutDays,
        riskWindowStart = riskWindowStart,
        startAnchor = startAnchor,
        riskWindowEnd = riskWindowEnd,
        endAnchor = endAnchor,
        dechallengeStopInterval = dechallengeStopInterval,
        dechallengeEvaluationWindow = dechallengeEvaluationWindow,
        covariateSettings = covariateSettings,
        caseCovariateSettings = caseCovariateSettings,
        casePreTargetDuration = casePreTargetDuration,
        casePostOutcomeDuration = casePostOutcomeDuration,
        includeTimeToEvent = includeTimeToEvent,
        includeDechallengeRechallenge = includeDechallengeRechallenge,
        includeTargetBaseline = includeTargetBaseline,
        includeRiskFactors = includeRiskFactors,
        includeCaseSeries = includeCaseSeries
      )

      legacyArgumentsProvided <- names(legacyArguments)[!vapply(legacyArguments, is.null, logical(1))]
      if (length(legacyArgumentsProvided) > 0) {
        stop(paste0(
          "Characterization v4 is required. The legacy Characterization v3 arguments are no longer supported: ",
          paste(legacyArgumentsProvided, collapse = ", "),
          ". Create `characterizationSettings` using Characterization v4 constructors and pass that object instead. Run ??Characterization::createCharacterizationSettings for more details"
        ))
      }

      if (is.null(characterizationSettings)) {
        stop("Characterization v4 is required. Create `characterizationSettings` using Characterization v4 constructors and pass that object instead.")
      }

      specifications <- super$createModuleSpecifications(
        moduleSpecifications = list(
          analysis = characterizationSettings,
          minCharacterizationMean = minCharacterizationMean,
          mode = mode,
          minSMD = minSMD,
          minCovariateCount = minCovariateCount,
          minTargetSize = minTargetSize,
          minCaseSize = minCaseSize,
          outputTable = outputTable
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
