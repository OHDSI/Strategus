# JobContext -------------
#' TODO: Create a formal definition of the job context
#' @title The job context definition that is used by the Strategus module
#' @export
#' @description
#' This object will define the job context for a module so it has a set
#' of well defined inputs to execute the module.
JobContext <- R6::R6Class(
  classname = "JobContext",
  public = list(
    #' @field sharedResources Shared resources for execution
    sharedResources = list(), # TODO: Revisit to break this into fields for cohorts, subsets, negative controls,
    #' @field settings Module settings
    settings = list(),
    #' @field moduleExecutionSettings Module execution settings
    moduleExecutionSettings = list()
  )
)

# StrategusModule -------------
#' @title StrategusModule defines the base class for each HADES Strategus module
#' @export
#' @description
#' Provides a base class for HADES Strategus modules to inherit
StrategusModule <- R6::R6Class(
  classname = "StrategusModule",
  public = list(
    #' @field tablePrefix The prefix to append to database tables
    #' that hold results for this module
    tablePrefix = "",
    #' @field jobContext The job context to use for execution.
    #' See JobContext class for more info
    jobContext = NA,
    #' @field moduleIndex The index of the module in the analysis specification.
    moduleIndex = 0,
    #' @field databaseId The database ID to use for the execution. TODO: This shouldn't be
    #' something the end-user worries about
    databaseId = "-1",
    #' @field moduleName The name of the module
    moduleName = "StrategusModuleBaseClass",
    initialize = function(jobContext, moduleIndex, databaseId) {
      checkmate::assertR6(jobContext, "JobContext")
      self$jobContext = jobContext
      # Carrying this over from the "RunModule.R" approach
      self$jobContext$moduleExecutionSettings$workSubFolder <- file.path(jobContext$moduleExecutionSettings$workFolder, sprintf("%s_%d", self$moduleName, moduleIndex))
      self$jobContext$moduleExecutionSettings$resultsSubFolder <- file.path(jobContext$moduleExecutionSettings$resultsFolder, sprintf("%s_%d", self$moduleName, moduleIndex))
      self$jobContext$moduleExecutionSettings$databaseId <- databaseId
    },
    #' @description Executes the module based on the job context
    #' @param connectionDetails The connection details to the database
    execute = function(connectionDetails) {
      private$.message('Executing...', self$moduleName)
    },
    #' @description Define the inputs to the module
    createAnalysisSettings = function() {
      # TODO - IMPLEMENT
      private$.message('Create analysis settings...')
      warning("NOT IMPLEMENTED")
    },
    #' @description Validate the analysis settings ahead of execution
    #' @param analysisSettings The analysis settings to validate
    validateAnalysisSettings = function(analysisSettings) {
      # TODO - IMPLEMENT
      private$.message('Validate analysis settings...')
      warning("NOT IMPLEMENTED")
    },
    #' @description Export the results to .csv from the work folder
    #' @param
    exportResults = function(connection, connectionDetails) {
      private$.message('exportResults...', self$moduleName)
    },
    #' @description Create the results schema for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    createResultsSchema = function(resultsConnectionDetails) {
      # TODO - IMPLEMENT
      private$.message('createResultsSchema...')
      warning("NOT IMPLEMENTED")
      private$.message(paste0("Table Prefix: ", self$tablePrefix))

      # TODO: Having a base implementation will be hard since we
      # need to know the location of the results data model to create it.
      # Here is the prior implementation with `dataModelExportPath` holding
      # the location where the resultsDataModelSpecification.csv was written
      # to the results folder.
      # connection <- DatabaseConnector::connect(resultsConnectionDetails)
      # on.exit(DatabaseConnector::disconnect(connection))
      # sql <- ResultModelManager::generateSqlSchema(csvFilepath = dataModelExportPath)
      # DatabaseConnector::renderTranslateExecuteSql(connection,
      #                                              sql,
      #                                              table_prefix = moduleInfo$TablePrefix,
      #                                              database_schema = jobContext$moduleExecutionSettings$resultsDatabaseSchema
      #)
    },
    #' @description Upload the results for the module
    #' @param connectionDetails The connection details to the results DB
    #' @param resultsFolder The folder with the results in .csv format
    uploadResults = function(connectionDetails) {
      # TODO - IMPLEMENT
      private$.message('uploadResults...')
      warning("NOT IMPLEMENTED")
    }
  ),
  private = list(
    .message = function(...) {
      rlang::inform(paste0(...))
    },
    # TODO - These methods were brought over from CG Module - need to revisit
    # these methods with respect to the job context
    #' @description Get an element, by name, from the shared resources in the job context
    #' @param jobContext The job context to use for execution. See JobContext class for more info
    .getSharedResourceByClassName = function(sharedResources, className) {
      returnVal <- NULL
      for (i in 1:length(sharedResources)) {
        if (className %in% class(sharedResources[[i]])) {
          returnVal <- sharedResources[[i]]
          break
        }
      }
      invisible(returnVal)
    },
    #' @description Create a cohort definition set from the job context
    #' @param jobContext The job context to use for execution. See JobContext class for more info
    .createCohortDefinitionSetFromJobContext = function() {
      jobContext <- self$jobContext
      cohortDefinitions <- list()
      if (length(jobContext$sharedResources) <= 0) {
        stop("No shared resources found")
      }
      cohortDefinitionSharedResource <- private$.getSharedResourceByClassName(
        sharedResources = jobContext$sharedResources,
        class = "CohortDefinitionSharedResources"
      )
      if (is.null(cohortDefinitionSharedResource)) {
        stop("Cohort definition shared resource not found!")
      }
      if ((is.null(cohortDefinitionSharedResource$subsetDefs) && !is.null(cohortDefinitionSharedResource$cohortSubsets)) ||
          (!is.null(cohortDefinitionSharedResource$subsetDefs) && is.null(cohortDefinitionSharedResource$cohortSubsets))) {
        stop("Cohort subset functionality requires specifying cohort subset definition & cohort subset identifiers.")
      }
      cohortDefinitionSet <- private$.getCohortDefinitionSetFromSharedResource(
        cohortDefinitionSharedResource = cohortDefinitionSharedResource,
        settings = jobContext$settings
      )
      return(cohortDefinitionSet)
    },
    #' @description Create a cohort definition set from the shared resource of the job context
    #' @param jobContext The job context to use for execution. See JobContext class for more info
    .getCohortDefinitionSetFromSharedResource = function(cohortDefinitionSharedResource, settings) {
      cohortDefinitions <- cohortDefinitionSharedResource$cohortDefinitions
      if (length(cohortDefinitions) <= 0) {
        stop("No cohort definitions found")
      }
      cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()
      for (i in 1:length(cohortDefinitions)) {
        cohortJson <- cohortDefinitions[[i]]$cohortDefinition
        cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
        cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = settings$generateStats))
        cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(
          cohortId = as.double(cohortDefinitions[[i]]$cohortId),
          cohortName = cohortDefinitions[[i]]$cohortName,
          sql = cohortSql,
          json = cohortJson,
          stringsAsFactors = FALSE
        ))
      }

      if (length(cohortDefinitionSharedResource$subsetDefs)) {
        subsetDefinitions <- lapply(cohortDefinitionSharedResource$subsetDefs, CohortGenerator::CohortSubsetDefinition$new)
        for (subsetDef in subsetDefinitions) {
          ind <- which(sapply(cohortDefinitionSharedResource$cohortSubsets, function(y) subsetDef$definitionId %in% y$subsetId))
          targetCohortIds <- unlist(lapply(cohortDefinitionSharedResource$cohortSubsets[ind], function(y) y$targetCohortId))
          cohortDefinitionSet <- CohortGenerator::addCohortSubsetDefinition(
            cohortDefinitionSet = cohortDefinitionSet,
            cohortSubsetDefintion = subsetDef,
            targetCohortIds = targetCohortIds
          )
        }
      }

      return(cohortDefinitionSet)
    },
    #' @description Determine if the job context has negative control outcomes
    #' @param jobContext The job context to use for execution. See JobContext class for more info
    .jobContextHasNegativeControlOutcomeSharedResource = function() {
      jobContext <- self$jobContext
      ncSharedResource <- private$.getSharedResourceByClassName(
        sharedResources = jobContext$sharedResources,
        className = "NegativeControlOutcomeSharedResources"
      )
      hasNegativeControlOutcomeSharedResource <- !is.null(ncSharedResource)
      invisible(hasNegativeControlOutcomeSharedResource)
    },
    #' @description Create the negative control outcomes from the job context
    #' @param jobContext The job context to use for execution. See JobContext class for more info
    .createNegativeControlOutcomeSettingsFromJobContext = function() {
      jobContext <- self$jobContext
      negativeControlSharedResource <- private$.getSharedResourceByClassName(
        sharedResources = jobContext$sharedResources,
        className = "NegativeControlOutcomeSharedResources"
      )
      if (is.null(negativeControlSharedResource)) {
        stop("Negative control outcome shared resource not found!")
      }
      negativeControlOutcomes <- negativeControlSharedResource$negativeControlOutcomes$negativeControlOutcomeCohortSet
      if (length(negativeControlOutcomes) <= 0) {
        stop("No negative control outcomes found")
      }
      negativeControlOutcomeCohortSet <- CohortGenerator::createEmptyNegativeControlOutcomeCohortSet()
      for (i in 1:length(negativeControlOutcomes)) {
        nc <- negativeControlOutcomes[[i]]
        negativeControlOutcomeCohortSet <- rbind(
          negativeControlOutcomeCohortSet,
          data.frame(
            cohortId = bit64::as.integer64(nc$cohortId),
            cohortName = nc$cohortName,
            outcomeConceptId = bit64::as.integer64(nc$outcomeConceptId)
          )
        )
      }
      invisible(list(
        cohortSet = negativeControlOutcomeCohortSet,
        occurrenceType = negativeControlSharedResource$negativeControlOutcomes$occurrenceType,
        detectOnDescendants = negativeControlSharedResource$negativeControlOutcomes$detectOnDescendants
      ))
    }
  )
)


# CohortGeneratorModule -------------
#' @title Module for generating cohorts against an OMOP CDM
#' @export
#' @description
#' Generates cohorts against the OMOP CDM
CohortGeneratorModule <- R6::R6Class(
  classname = "CohortGeneratorModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The prefix to append to database tables
    #' that hold results for this module
    tablePrefix = "cg_",
    moduleName = "CohortGeneratorModule",
    cohortDefinitionSet = data.frame(),
    initialize = function(jobContext, moduleIndex, databaseId) {
      super$initialize(
        jobContext = jobContext,
        moduleIndex = moduleIndex,
        databaseId = databaseId
      )
      self$cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext()
    },
    #' @description Generates the cohorts
    #' @param jobContext The job context to use for execution. See JobContext class for more info
    execute = function(connectionDetails) {
      super$execute(connectionDetails)
      jobContext <- self$jobContext

      cohortDefinitionSet <- self$cohortDefinitionSet

      # Establish the connection and ensure the cleanup is performed
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))

      # Create the cohort tables
      CohortGenerator::createCohortTables(
        connection = connection,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
        incremental = jobContext$settings$incremental
      )

      # Generate the cohorts
      cohortsGenerated <- CohortGenerator::generateCohortSet(
        connection = connection,
        cohortDefinitionSet = cohortDefinitionSet,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
        incremental = jobContext$settings$incremental,
        incrementalFolder = jobContext$moduleExecutionSettings$workSubFolder
      )

      # Generate any negative controls
      if (private$.jobContextHasNegativeControlOutcomeSharedResource()) {
        negativeControlOutcomeSettings <- private$.createNegativeControlOutcomeSettingsFromJobContext()

        CohortGenerator::generateNegativeControlOutcomeCohorts(
          connection = connection,
          cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
          cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
          cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
          negativeControlOutcomeCohortSet = negativeControlOutcomeSettings$cohortSet,
          tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema,
          occurrenceType = negativeControlOutcomeSettings$occurrenceType,
          detectOnDescendants = negativeControlOutcomeSettings$detectOnDescendants,
          incremental = jobContext$settings$incremental,
          incrementalFolder = jobContext$moduleExecutionSettings$workSubFolder
        )
      }

      self$exportResults(
        connectionDetails = connectionDetails,
        cohortsGenerated = cohortsGenerated
      )
    },
    exportResults = function(connection = NULL, connectionDetails = NULL, cohortsGenerated = NULL) {
      if (is.null(connection) && is.null(connectionDetails)) {
        stop("You must provide either a database connection or the connection details.")
      }

      if (is.null(connection)) {
        # Establish the connection and ensure the cleanup is performed
        connection <- DatabaseConnector::connect(connectionDetails)
        on.exit(DatabaseConnector::disconnect(connection))
      }

      jobContext <- self$jobContext
      cohortDefinitionSet <- self$cohortDefinitionSet

      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      if (!dir.exists(resultsFolder)) {
        dir.create(resultsFolder, recursive = TRUE)
      }


      # Save the generation information
      if (!is.null(cohortsGenerated) && nrow(cohortsGenerated) > 0) {
        cohortsGenerated$databaseId <- jobContext$moduleExecutionSettings$databaseId
        # Remove any cohorts that were skipped
        cohortsGenerated <- cohortsGenerated[toupper(cohortsGenerated$generationStatus) != "SKIPPED", ]
        cohortsGeneratedFileName <- file.path(resultsFolder, "cohort_generation.csv")
        if (jobContext$settings$incremental) {
          # Format the data for saving
          names(cohortsGenerated) <- SqlRender::camelCaseToSnakeCase(names(cohortsGenerated))
          CohortGenerator::saveIncremental(
            data = cohortsGenerated,
            fileName = cohortsGeneratedFileName,
            cohort_id = cohortsGenerated$cohort_id
          )
        } else {
          CohortGenerator::writeCsv(
            x = cohortsGenerated,
            file = cohortsGeneratedFileName
          )
        }
      }

      cohortCounts <- CohortGenerator::getCohortCounts(
        connection = connection,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        cohortDefinitionSet = cohortDefinitionSet,
        databaseId = jobContext$moduleExecutionSettings$databaseId
      )

      # Filter to columns in the results data model
      cohortCounts <- private$.filterCohortCountsColumns(cohortCounts)

      CohortGenerator::writeCsv(
        x = cohortCounts,
        file = file.path(resultsFolder, "cohort_count.csv")
      )

      # Insert the inclusion rule names before exporting the stats tables
      CohortGenerator::exportCohortStatsTables(
        connection = connection,
        cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortStatisticsFolder = resultsFolder,
        snakeCaseToCamelCase = FALSE,
        fileNamesInSnakeCase = TRUE,
        incremental = jobContext$settings$incremental,
        databaseId = jobContext$moduleExecutionSettings$databaseId,
        cohortDefinitionSet = cohortDefinitionSet
      )

      # Massage and save the cohort definition set
      colsToRename <- c("cohortId", "cohortName", "sql", "json")
      colInd <- which(names(cohortDefinitionSet) %in% colsToRename)
      cohortDefinitions <- cohortDefinitionSet
      names(cohortDefinitions)[colInd] <- c("cohortDefinitionId", "cohortName", "sqlCommand", "json")
      cohortDefinitions$description <- ""
      CohortGenerator::writeCsv(
        x = cohortDefinitions,
        file = file.path(resultsFolder, "cohort_definition.csv")
      )

      # Generate any negative controls
      if (private$.jobContextHasNegativeControlOutcomeSharedResource()) {
        negativeControlOutcomeSettings <- private$.createNegativeControlOutcomeSettingsFromJobContext()

        cohortCountsNegativeControlOutcomes <- CohortGenerator::getCohortCounts(
          connection = connection,
          cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
          cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
          databaseId = jobContext$moduleExecutionSettings$databaseId,
          cohortIds = negativeControlOutcomeSettings$cohortSet$cohortId
        )

        CohortGenerator::writeCsv(
          x = cohortCountsNegativeControlOutcomes,
          file = file.path(resultsFolder, "cohort_count_neg_ctrl.csv")
        )
      }


      # Set the table names in resultsDataModelSpecification.csv
      resultsDataModel <- CohortGenerator::readCsv(
        file = "resultsDataModelSpecification.csv",
        warnOnCaseMismatch = FALSE
      )
      newTableNames <- paste0(self$tablePrefix, resultsDataModel$tableName)
      file.rename(
        file.path(resultsFolder, paste0(unique(resultsDataModel$tableName), ".csv")),
        file.path(resultsFolder, paste0(unique(newTableNames), ".csv"))
      )
      resultsDataModel$tableName <- newTableNames
      CohortGenerator::writeCsv(
        x = resultsDataModel,
        file = file.path(resultsFolder, "resultsDataModelSpecification.csv"),
        warnOnCaseMismatch = FALSE,
        warnOnFileNameCaseMismatch = FALSE,
        warnOnUploadRuleViolations = FALSE
      )
      private$.message(paste("Results available at:", zipFile))
    },
    createResultsSchema = function(resultsConnectionDetails) {
      super$createResultsSchema(resultsConnectionDetails)
    }
  ),
  private = list(
    .filterCohortCountsColumns = function(cohortCounts) {
      # Filter to columns in the results data model
      return(cohortCounts[c("databaseId", "cohortId", "cohortEntries", "cohortSubjects")])
    }
  )
)
