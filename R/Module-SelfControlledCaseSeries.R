# SelfControlledCaseSeriesModule -------------
#' @title Self-Controlled Case Series design with the \href{https://ohdsi.github.io/SelfControlledCaseSeries/}{HADES SelfControlledCaseSeries Package}
#' @export
#' @description
#' Module for performing Self-Controlled Case Series (SCCS) analyses
#' against the OMOP Common Data Model.
SelfControlledCaseSeriesModule <- R6::R6Class(
  classname = "SelfControlledCaseSeriesModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix for results tables
    tablePrefix = "sccs_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the SelfControlledCaseSeries package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      sccsMultiThreadingSettings <- SelfControlledCaseSeries::createDefaultSccsMultiThreadingSettings(jobContext$moduleExecutionSettings$maxCores)

      # Provide hook to allow for overriding the number of threads
      # used for database operations
      getDbSccsDataThreads <- as.integer(getOption("strategus.SelfControlledCaseSeriesModule.getDbSccsDataThreads"))
      if (isTRUE(getDbSccsDataThreads > 0)) {
        private$.message(paste0("Detected strategus.SelfControlledCaseSeriesModule.getDbSccsDataThreads - setting value to: ", getDbSccsDataThreads))
        sccsMultiThreadingSettings$getDbSccsDataThreads <- getDbSccsDataThreads
      }

      args <- jobContext$settings
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
      args$exposureDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$exposureTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outcomeDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$outcomeTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$nestingCohortDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$nestingCohortTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$customCovariateDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$customCovariateTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outputFolder <- jobContext$moduleExecutionSettings$workSubFolder
      args$sccsMultiThreadingSettings <- sccsMultiThreadingSettings
      args$sccsDiagnosticThresholds <- NULL
      do.call(SelfControlledCaseSeries::runSccsAnalyses, args)

      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      SelfControlledCaseSeries::exportToCsv(
        outputFolder = jobContext$moduleExecutionSettings$workSubFolder,
        exportFolder = exportFolder,
        databaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        sccsDiagnosticThresholds = jobContext$settings$sccsDiagnosticThresholds
      )
      # TODO: Removing this to make the upload easier
      # unlink(file.path(exportFolder, sprintf("Results_%s.zip", jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId)))

      resultsDataModel <- self$getResultsDataModelSpecification()
      resultsDataModel <- resultsDataModel[file.exists(file.path(exportFolder, paste0(resultsDataModel$tableName, ".csv"))), ]
      if (any(!startsWith(resultsDataModel$tableName, self$tablePrefix))) {
        stop("Table names do not have required prefix")
      }
      CohortGenerator::writeCsv(
        x = resultsDataModel,
        file = file.path(exportFolder, "resultsDataModelSpecification.csv"),
        warnOnFileNameCaseMismatch = FALSE
      )

      private$.message(paste("Results available at:", exportFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = "") {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      # Note: not passing the tablePrefix argument to
      # createResultsDataModel since the SCCS results
      # model already contains the "sccs_" table prefix
      SelfControlledCaseSeries::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsDatabaseSchema,
      )
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries"),
        warnOnCaseMismatch = FALSE
      )

      # add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      # TODO: This is something SCCS does differently.
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

      SelfControlledCaseSeries::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        zipFileName = zipFileName,
        purgeSiteDataBeforeUploading = FALSE
      )
    },
    #' @description Creates the SelfControlledCaseSeries Module Specifications
    #' @param sccsAnalysisList description
    #' @param exposuresOutcomeList description
    #' @param analysesToExclude description
    #' @param combineDataFetchAcrossOutcomes description
    #' @param sccsDiagnosticThresholds description
    createModuleSpecifications = function(sccsAnalysisList,
                                          exposuresOutcomeList,
                                          analysesToExclude = NULL,
                                          combineDataFetchAcrossOutcomes = FALSE,
                                          sccsDiagnosticThresholds = SelfControlledCaseSeries::createSccsDiagnosticThresholds()) {
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
    #' @param moduleSpecifications The SelfControlledCaseSeries module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    },
    #' @description Partitions the module specifications into smaller jobs
    #' @template analysisSpecifications
    #' @param specificationFolder A directory where the partitioned jsons will be saved to
    partitionModuleSpecifications = function(analysisSpecifications, specificationFolder) {

      moduleVector <- unlist(lapply(analysisSpecifications$moduleSpecifications, function(ms) ms$module))
      selfInd <- which(moduleVector == self$moduleName)
      if(length(selfInd) == 0){
        message(paste0('No specification found for ',self$moduleName))
        invisible(return(FALSE))
      }
      selfSpecification <- analysisSpecifications$moduleSpecifications[[selfInd]]


      #outcomeId, nestingCohortId, exposure:exposureId/exposureIdRef/trueEffectSize

      # Split by nestingCohortId and exposureId or by outcomeId

      eoComponents <- .extractExposuresOutcomeComponents(selfSpecification$settings$exposuresOutcomeList)

      convertNulls <- function(x){
        if(x == 'null'){
          return(NULL)
        }
        return(as.double(x))
      }

      # create partition list for each exposure and outcome
      exposureOI <- unique(eoComponents$exposureId)
      listOfEO <- list()
      for(i in 1:length(exposureOI)){
        subset <- eoComponents[eoComponents$exposureId== exposureOI[i],]
        mainComps <- unique(subset[,c('outcomeId','nestingCohortId')])

        tempList <- list()
        for(j in 1:nrow(mainComps)){

          ind <- which(subset$outcomeId == mainComps$outcomeId[j] & subset$nestingCohortId == mainComps$nestingCohortId[j])

          tempList[[j]] <- list(
            outcomeId = mainComps$outcomeId[j],
            nestingCohortId = convertNulls(mainComps$nestingCohortId[j]),
            exposures = lapply(ind, function(k){
              res <- list(
                exposureId = subset$exposureId[k],
                exposureIdRef = subset$exposureIdRef[k],
                trueEffectSize = convertNulls(subset$trueEffectSize[k])
              )
              class(res) <- 'Exposure'
              return(res)
            }
            )
          )
        }
        listOfEO[[length(listOfEO) + 1]] <- tempList
      }


      # create base setting with just shared resources and self spec
      baseSettings <- list(
        sharedResources = analysisSpecifications$sharedResources,
        moduleSpecifications = list(selfSpecification)
      )

      # now save each json spec
      if(!dir.exists(specificationFolder)){
        dir.create(specificationFolder, recursive = T)
      }

      fileVector <- c()
      for(i in 1:length(listOfEO)){
        tempSettings <- baseSettings
        tempSettings$moduleSpecifications[[1]]$settings$exposuresOutcomeList <- listOfEO[[i]]

        specHashId <- digest::digest2int(
          x = as.character(ParallelLogger::convertSettingsToJson(tempSettings$moduleSpecifications))
        )
        tempFilePath <- file.path(specificationFolder, paste0('spec_',exposureOI[i],'_',specHashId,'.json'))

        fileVector <- c(fileVector,tempFilePath)

        # save as spec_i.json - same name for each module but will be
        # in a different folder
        ParallelLogger::saveSettingsToJson(
          object = tempSettings,
          fileName = tempFilePath
        )
      }

      # TODO: could return the parititioned modelDesigns or the list of tempSettings
      #       or a status/message
      invisible(return(fileVector))


    }
  )
)


.extractExposuresOutcomeComponents <- function(exposuresOutcomeList){
  do.call('rbind', lapply(exposuresOutcomeList, function(eol){
    merge(
      data.frame(
        outcomeId = eol$outcomeId,
        nestingCohortId = ifelse(is.null(eol$nestingCohortId), 'null', eol$nestingCohortId)
      ),
      do.call('rbind', lapply(eol$exposures,
                              function(x) data.frame(
                                exposureId = x$exposureId,
                                exposureIdRef = x$exposureIdRef,
                                trueEffectSize = ifelse(is.null(x$trueEffectSize), 'null', x$trueEffectSize))
      ))
    )
  }
  ))
}

