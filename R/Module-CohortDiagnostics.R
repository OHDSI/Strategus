# CohortDiagnosticsModule -------------
#' @title Evaluate phenotypes with the \href{https://ohdsi.github.io/CohortDiagnostics/}{HADES CohortDiagnostics Package}
#' @export
#' @description
#' Development and evaluation of phenotype algorithms
#' against the OMOP Common Data Model.
CohortDiagnosticsModule <- R6::R6Class(
  classname = "CohortDiagnosticsModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix to append to results tables
    tablePrefix = "cd_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the CohortDiagnostics package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext()

      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      args <- jobContext$settings
      args$cohortDefinitionSet <- cohortDefinitionSet
      args$exportFolder <- exportFolder
      args$databaseId <- jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
      args$cohortDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$cohortTableNames <- jobContext$moduleExecutionSettings$cohortTableNames
      args$incrementalFolder <- jobContext$moduleExecutionSettings$workSubFolder
      args$minCellCount <- jobContext$moduleExecutionSettings$minCellCount
      args$cohortIds <- jobContext$moduleExecutionSettings$cohortIds
      do.call(CohortDiagnostics::executeDiagnostics, args)

      # TODO: Removing this to make the upload easier
      # unlink(file.path(exportFolder, sprintf("Results_%s.zip", jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId)))

      # NOTE: CD exports data without the table prefix and this section
      # will manipulate the files to include the table prefix
      resultsDataModelSpecification <- private$.getResultsDataModelSpecification()
      resultsDataModelSpecification <- resultsDataModelSpecification[file.exists(file.path(exportFolder, paste0(resultsDataModelSpecification$tableName, ".csv"))), ]
      newTableNames <- paste0(self$tablePrefix, resultsDataModelSpecification$tableName)
      file.rename(
        file.path(exportFolder, paste0(unique(resultsDataModelSpecification$tableName), ".csv")),
        file.path(exportFolder, paste0(unique(newTableNames), ".csv"))
      )
      resultsDataModelSpecification$tableName <- newTableNames
      CohortGenerator::writeCsv(
        x = resultsDataModelSpecification,
        file.path(exportFolder, "resultsDataModelSpecification.csv"),
        warnOnFileNameCaseMismatch = FALSE
      )

      private$.message(paste("Results available at:", exportFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = self$tablePrefix) {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      CohortDiagnostics::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsDatabaseSchema,
        tablePrefix = tablePrefix
      )
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- private$.getResultsDataModelSpecification()
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

      # TODO: This is something CD does differently.
      # Find the results zip file in the results sub folder
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

      CohortDiagnostics::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        tablePrefix = self$tablePrefix,
        zipFileName = zipFileName,
        purgeSiteDataBeforeUploading = FALSE
      )
    },
    #' @description Creates the CohortDiagnostics Module Specifications
    #'
    #'
    #' @param cohortIds A list of cohort IDs to use when running the CohortDiagnostics.
    #' Default is NULL which will use all cohorts present in the cohort definition set
    #' in the analysis specification
    #' @param runInclusionStatistics      Generate and export statistic on the cohort inclusion rules?
    #' @param runIncludedSourceConcepts   Generate and export the source concepts included in the cohorts?
    #' @param runOrphanConcepts           Generate and export potential orphan concepts?
    #' @param runTimeSeries               Generate and export the time series diagnostics?
    #' @param runVisitContext             Generate and export index-date visit context?
    #' @param runBreakdownIndexEvents     Generate and export the breakdown of index events?
    #' @param runIncidenceRate            Generate and export the cohort incidence  rates?
    #' @param runCohortRelationship       Generate and export the cohort relationship? Cohort relationship checks the temporal
    #'                                    relationship between two or more cohorts.
    #' @param runTemporalCohortCharacterization   Generate and export the temporal cohort characterization?
    #'                                            Only records with values greater than 0.001 are returned.
    #' @param temporalCovariateSettings   Either an object of type \code{covariateSettings} as created using one of
    #'                                    the createTemporalCovariateSettings function in the FeatureExtraction package, or a list
    #'                                    of such objects.
    #' @param minCharacterizationMean     The minimum mean value for characterization output. Values below this will be cut off from output. This
    #'                                    will help reduce the file size of the characterization output, but will remove information
    #'                                    on covariates that have very low values. The default is 0.001 (i.e. 0.1 percent)
    #' @param irWashoutPeriod             Number of days washout to include in calculation of incidence rates - default is 0
    createModuleSpecifications = function(cohortIds = NULL,
                                          runInclusionStatistics = TRUE,
                                          runIncludedSourceConcepts = TRUE,
                                          runOrphanConcepts = TRUE,
                                          runTimeSeries = FALSE,
                                          runVisitContext = TRUE,
                                          runBreakdownIndexEvents = TRUE,
                                          runIncidenceRate = TRUE,
                                          runCohortRelationship = TRUE,
                                          runTemporalCohortCharacterization = TRUE,
                                          temporalCovariateSettings = private$.getDefaultCovariateSettings(),
                                          minCharacterizationMean = 0.01,
                                          irWashoutPeriod = 0) {
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
    #' @param moduleSpecifications The CohortIncidence module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
      },
    #' @description Paritions the module specifications into smaller jobs
    #' @template analysisSpecifications
    #' @param specificationFolder A directory where the partitioned jsons will be saved to
    partitionModuleSpecifications = function(analysisSpecifications, specificationFolder) {

      moduleVector <- unlist(lapply(analysisSpecifications$moduleSpecifications, function(ms) ms$module))
      selfInd <- which(moduleVector == self$moduleName)
      if(sum(selfInd) == 0){
        message(paste0('No specification found for ',self$moduleName))
        invisible(return(FALSE))
      }
      selfSpecification <- analysisSpecifications$moduleSpecifications[[selfInd]]

      # save the full spec as it is because we do not need to split
      # create base setting with just shared resources and self spec
      baseSettings <- list(
        sharedResources = analysisSpecifications$sharedResources,
        moduleSpecifications = selfSpecification
      )

      specHashId <- digest::digest2int(
        x = as.character(ParallelLogger::convertSettingsToJson(selfSpecification))
      )

      # now save the fill json spec
      if(!dir.exists(specificationFolder)){
        dir.create(specificationFolder, recursive = T)
      }

      # save as spec_1.json - same name for each module but will be
      # in a different folder
      ParallelLogger::saveSettingsToJson(
        object = baseSettings,
        fileName = file.path(specificationFolder, paste0('spec_',specHashId,'.json'))
      )

      # TODO: could return the parititioned modelDesigns or the list of tempSettings
      #       or a status/message
      invisible(return(file.path(specificationFolder, paste0('spec_',specHashId,'.json'))))
    }
  ),
  private = list(
    .getDefaultCovariateSettings = function() {
      covariateSettings <- '
      {
        "temporal": true,
        "temporalSequence": false,
        "DemographicsGender": true,
        "DemographicsAge": true,
        "DemographicsAgeGroup": true,
        "DemographicsRace": true,
        "DemographicsEthnicity": true,
        "DemographicsIndexYear": true,
        "DemographicsIndexMonth": true,
        "DemographicsPriorObservationTime": true,
        "DemographicsPostObservationTime": true,
        "DemographicsTimeInCohort": true,
        "DemographicsIndexYearMonth": true,
        "ConditionOccurrence": true,
        "ConditionEraStart": true,
        "ConditionEraOverlap": true,
        "ConditionEraGroupOverlap": true,
        "DrugEraStart": true,
        "DrugEraGroupOverlap": true,
        "ProcedureOccurrence": true,
        "DeviceExposure": true,
        "Measurement": true,
        "Observation": true,
        "CharlsonIndex": true,
        "Dcsi": true,
        "Chads2": true,
        "Chads2Vasc": true,
        "temporalStartDays": [-9999, -365, -180, -30, -365, -30, 0, 1, 31, -9999],
        "temporalEndDays": [0, 0, 0, 0, -31, -1, 0, 30, 365, 9999],
        "includedCovariateConceptIds": [],
        "addDescendantsToInclude": false,
        "excludedCovariateConceptIds": [],
        "addDescendantsToExclude": false,
        "includedCovariateIds": [],
        "attr_class": "covariateSettings",
        "attr_fun": "getDbDefaultCovariateData"
      }
        '
      ParallelLogger::convertJsonToSettings(covariateSettings)
    },
    .getResultsDataModelSpecification = function() {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = system.file(
          file.path("settings", "resultsDataModelSpecification.csv"),
          package = "CohortDiagnostics"
        ),
        warnOnCaseMismatch = FALSE
      )
      return(resultsDataModelSpecification)
    }
  )
)
