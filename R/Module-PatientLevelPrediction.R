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

      jobContext$settings <- private$.setCovariateSchemaTable(
        modelDesignList = jobContext$settings$modelDesignList,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      )

      # run the models
      PatientLevelPrediction::runMultiplePlp(
        databaseDetails = databaseDetails,
        modelDesignList = jobContext$settings,
        cohortDefinitions = cohortDefinitionSet,
        saveDirectory = workFolder
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
    createModuleSpecifications = function(modelDesignList) {
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
    },
    #' @description Summarize the uploaded results for the module
    #' @template resultsConnectionDetails
    #' @template resultsDataModelSettings
    summarizeResults = function(resultsConnectionDetails, resultsDataModelSettings) {
      # initialize checks

      schema <- resultsDataModelSettings$resultsDatabaseSchema
      prefix <- self$tablePrefix

      checks <- c()

      # connect to resultsConnectionDetails
      connectionHandler <- ResultModelManager::ConnectionHandler$new(
        connectionDetails = resultsConnectionDetails
      )

      # get cohort count
      result <- connectionHandler$queryDb(
        "select count(distinct cohort_id) as N from @schema.@prefixcohorts",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'cohorts',database = '-', check = 'count count', value = result$n))

      # get database count
      result <- connectionHandler$queryDb(
        "select count(distinct database_id) as N from @schema.@prefixdatabase_meta_data",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'database_meta_data',database = '-', check = 'database count', value = result$n))

      # get tar count
      result <- connectionHandler$queryDb(
        "select count(distinct tar_id) as N from @schema.@prefixtars",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'tars',database = '-', check = 'tar count', value = result$n))

      # get population count
      result <- connectionHandler$queryDb(
        "select count(distinct population_setting_id) as N from @schema.@prefixpopulation_settings",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'population_settings',database = '-', check = 'population count', value = result$n))

      # get covariate count
      result <- connectionHandler$queryDb(
        "select count(distinct covariate_setting_id) as N from @schema.@prefixcovariate_settings",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'covariate_settings',database = '-', check = 'covariate count', value = result$n))

      # get model count
      result <- connectionHandler$queryDb(
        "select count(distinct model_setting_id) as N from @schema.@prefixmodel_settings",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'model_settings',database = '-', check = 'model count', value = result$n))

      # get split count
      result <- connectionHandler$queryDb(
        "select count(distinct split_setting_id) as N from @schema.@prefixsplit_settings",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'split_settings',database = '-', check = 'split count', value = result$n))

      # get plp_data_settings count
      result <- connectionHandler$queryDb(
        "select count(distinct plp_data_setting_id) as N from @schema.@prefixplp_data_settings",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'plp_data_settings',database = '-', check = 'data setting count', value = result$n))

      # get tidy_covariates_setting count
      result <- connectionHandler$queryDb(
        "select count(distinct tidy_covariates_setting_id) as N from @schema.@prefixtidy_covariates_settings",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'tidy_covariates_settings',database = '-', check = 'tidy covariates count', value = result$n))

      # get sample_settings count
      result <- connectionHandler$queryDb(
        "select count(distinct sample_setting_id) as N from @schema.@prefixsample_settings",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'sample_settings',database = '-', check = 'sample setting count', value = result$n))


      # get model_designs count
      result <- connectionHandler$queryDb(
        "select count(distinct model_design_id) as N from @schema.@prefixmodel_designs",
        schema = schema,
        prefix = prefix
      )
      checks <- rbind(checks, data.frame(table = 'model_designs',database = '-', check = 'model design count', value = result$n))

      # get diagnostic count of cohorts per database
      result <- connectionHandler$queryDb(
        "select
      dd.database_meta_data_id as database_id,
      count(distinct md.target_id) as t_n,
      count(distinct md.outcome_id) as o_n
      from @schema.@prefixdiagnostics d
      inner join
      @schema.@prefixmodel_designs md
      on
      md.model_design_id = d.model_design_id
      inner join
      @schema.@prefixdatabase_details dd
      on d.database_id = dd.database_id
      group by dd.database_id;",
        schema = schema,
        prefix = prefix
      )
      if(nrow(result)>0){
        checks <- rbind(checks, data.frame(table = 'diagnostic',database = result$databaseId, check = 'target count', value = result$tN))
        checks <- rbind(checks, data.frame(table = 'diagnostic',database = result$databaseId, check = 'outcome count', value = result$oN))
      }


      # get performance count of cohorts per database
      result <- connectionHandler$queryDb(
        "select
      dd.database_meta_data_id as database_id,
      count(distinct p.target_id) as t_n,
      count(distinct p.outcome_id) as o_n
      from @schema.@prefixperformances p
      inner join
      @schema.@prefixdatabase_details dd
      on p.development_database_id = dd.database_id
      group by dd.database_id;",
        schema = schema,
        prefix = prefix
      )
      if(nrow(result)>0){
        checks <- rbind(checks, data.frame(table = 'performance',database = result$databaseId, check = 'target count', value = result$tN))
        checks <- rbind(checks, data.frame(table = 'performance',database = result$databaseId, check = 'outcome count', value = result$oN))
      }

      message('PatientLevelPrediction uploaded result summary:')
      # print out the checksprint(checks)

      return(checks)
    },
    #' @description Paritions the module specifications into smaller jobs
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
      
      modelDesignList <- selfSpecification$settings$modelDesignList
      #TODO can modelDesignList be a single modelDesign?  If so, check and cast to list
  
      # split the modelDesign list by targetId
      targetIds <- unlist(lapply(modelDesignList, function(md){md$targetId}))
      
      # for each targetId create a seperate modelDesignList
      listOfmodelDesignList <- lapply(
        X = unique(targetIds), 
        FUN = function(tId){
          modelDesignList[which(tId == targetIds)]
      })
      
      # create base setting with just shared resources and self spec
      baseSettings <- list(
        sharedResources = analysisSpecifications$sharedResources,
        moduleSpecifications = list(selfSpecification)
      )
      
      # now save each json spec 
      if(!dir.exists(specificationFolder)){
        dir.create(specificationFolder, recursive = T)
      }

      for(i in 1:length(listOfmodelDesignList)){
        # replace complete modelList with the small modelList 
        # for each partition of modelList
        # TODO: could also reduce the sharedResources cohort definitions to just
        # those needed for the partition
        tempSettings <- baseSettings
        tempSettings$moduleSpecifications[[1]]$settings$modelDesignList <- listOfmodelDesignList[[i]]
        
        # save as spec_i.json - same name for each module but will be
        # in a different folder
        ParallelLogger::saveSettingsToJson(
          object = tempSettings, 
          fileName = file.path(specificationFolder, paste0('spec_',unique(targetIds)[i],'.json'))
          )
      }
      
      # TODO: could return the parititioned modelDesigns or the list of tempSettings
      #       or a status/message
      invisible(return(TRUE))
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
