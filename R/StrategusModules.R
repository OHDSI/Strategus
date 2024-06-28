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
    # TODO: Remove the initialize function and just pass to methods instead.
    # Need to handle the sub folders differently.
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
            cohortId = as.numeric(nc$cohortId),
            cohortName = nc$cohortName,
            outcomeConceptId = as.numeric(nc$outcomeConceptId)
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
      negativeControlOutcomeSettings <- private$.createNegativeControlOutcomeSettingsFromJobContext()
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      if (!dir.exists(resultsFolder)) {
        dir.create(resultsFolder, recursive = TRUE)
      }

      CohortGenerator::runCohortGeneration(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        cohortTableNames = jobContext$moduleExecutionSettings$cohortTableNames,
        cohortDefinitionSet = self$cohortDefinitionSet,
        negativeControlOutcomeCohortSet = negativeControlOutcomeSettings$cohortSet,
        occurrenceType = negativeControlOutcomeSettings$occurrenceType,
        detectOnDescendants = negativeControlOutcomeSettings$detectOnDescendants,
        outputFolder = resultsFolder,
        databaseId = jobContext$moduleExecutionSettings$databaseId,
        incremental = jobContext$settings$incremental,
        incrementalFolder = jobContext$moduleExecutionSettings$workSubFolder
      )

      private$.message(paste("Results available at:", resultsFolder))
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
