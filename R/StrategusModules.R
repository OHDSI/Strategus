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
    #' @field databaseId The database ID to use for the execution.
    #' TODO: This shouldn't be something the end-user worries about
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
      private$.message('EXECUTING: ', self$moduleName)
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
    #' TODO: This is BAD for the base class since the settings passed in here
    #' (jobContext$settings) are assumed to be for the cohort generator.
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
    #' TODO: This is BAD for the base class since the settings passed in here are assumed
    #' to be for the cohort generator.
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


# StrategusModuleSettings -------------
#' @title Class for creating settings that are used by a Strategus Module
#' @export
#' @description
#' Class for creating settings that are used by a Strategus Module. The
#' main motivator for this class is that R6 does not provide a way
#' to create static methods.The settings class should have an empty
#' constructor while the main StrategusModule will likely need some
#' type of inputs to the constructor.
#' TODO: Revisit this with the working group.
StrategusModuleSettings <- R6::R6Class(
  classname = "# StrategusModuleSettings",
  public = list(
    #' @description Base function for creating the module settings object.
    #' Each module will have its own implementation and this base class method
    #' will be used to ensure the class of the specifications is set properly.
    #' @param className The class name for the module specifications
    #' @param moduleSpecifications The module specifications
    createModuleSpecifications = function(className, moduleSpecifications) {
      class(moduleSpecifications) <- c(className, "ModuleSpecifications")
      return(moduleSpecifications)
    },
    #' @description Base function for creating the shared resources settings object.
    #' Each module will have its own implementation if it needs to create
    #' a shared resource.
    #' @param className The class name for the shared resources specifications
    #' @param sharedResourceSpecifications The shared resources specifications
    createSharedResourcesSpecifications = function(className, sharedResourcesSpecifications) {
      class(sharedResourcesSpecifications) <- c(className, "SharedResources")
      return(sharedResourcesSpecifications)
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
      browser()
      self$cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext()
    },
    #' @description Generates the cohorts
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
  )
)


# CohortGeneratorModuleSettings -------------
#' @title Create settings for use with the @seealso [CohortGeneratorModule]
#' @export
#' @description
#' Module settings used by the @seealso [CohortGeneratorModule]
CohortGeneratorModuleSettings <- R6::R6Class(
  classname = "CohortGeneratorModuleSettings",
  inherit = StrategusModuleSettings,
  public = list(
    #' @description Creates the CohortGenerator Module Specifications
    createModuleSpecifications = function(incremental = TRUE,
                                          generateStats = TRUE) {
      analysis <- list()
      for (name in names(formals(self$createModuleSpecifications))) {
        analysis[[name]] <- get(name)
      }

      specifications <- super$createModuleSpecifications(
        className = "CohortGeneratorModuleSpecifications",
        moduleSpecifications = list(
          module = "CohortGeneratorModule",
          settings = analysis
        )
      )
      return(specifications)
    },
    #' @description Create shared specifications for the cohort definition set
    createCohortSharedResourceSpecifications = function(cohortDefinitionSet) {
      if (!CohortGenerator::isCohortDefinitionSet(cohortDefinitionSet)) {
        stop("cohortDefinitionSet is not properly defined")
      }

      subsetDefinitions <- CohortGenerator::getSubsetDefinitions(cohortDefinitionSet)
      if (length(subsetDefinitions) > 0) {
        # Filter the cohort definition set to the "parent" cohorts.
        parentCohortDefinitionSet <- cohortDefinitionSet[!cohortDefinitionSet$isSubset, ]
      } else {
        parentCohortDefinitionSet <- cohortDefinitionSet
      }

      sharedResource <- list()
      cohortDefinitionSetFiltered <- private$.listafy(parentCohortDefinitionSet)
      sharedResource["cohortDefinitions"] <- list(cohortDefinitionSetFiltered)

      if (length(subsetDefinitions)) {
        # Subset definitions
        subsetDefinitionsJson <- lapply(subsetDefinitions, function(x) {
          x$toJSON()
        })
        sharedResource["subsetDefs"] <- list(subsetDefinitionsJson)

        # Filter to the subsets
        subsetCohortDefinitionSet <- cohortDefinitionSet[cohortDefinitionSet$isSubset, ]
        subsetIdMapping <- list()
        for (i in 1:nrow(subsetCohortDefinitionSet)) {
          idMapping <- list(
            cohortId = subsetCohortDefinitionSet$cohortId[i],
            subsetId = subsetCohortDefinitionSet$subsetDefinitionId[i],
            targetCohortId = subsetCohortDefinitionSet$subsetParent[i]
          )
          subsetIdMapping[[i]] <- idMapping
        }
        sharedResource["cohortSubsets"] <- list(subsetIdMapping)
      }

      sharedResource <- super$createSharedResourcesSpecifications(
        className = "CohortDefinitionSharedResources",
        sharedResourcesSpecifications = sharedResource
      )
      return(sharedResource)
    },
    #' @description Create shared specifications for the negative control outcomes cohort set
    createNegativeControlOutcomeCohortSharedResourceSpecifications = function(negativeControlOutcomeCohortSet,
                                                                              occurrenceType,
                                                                              detectOnDescendants) {
      negativeControlOutcomeCohortSet <- apply(negativeControlOutcomeCohortSet, 1, as.list)
      sharedResource <- list(
        negativeControlOutcomes = list(
          negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
          occurrenceType = occurrenceType,
          detectOnDescendants = detectOnDescendants
        )
      )
      sharedResource <- super$createSharedResourcesSpecifications(
        className = "NegativeControlOutcomeSharedResources",
        sharedResourcesSpecifications = sharedResource
      )
      return(sharedResource)
    }
  ),
  private = list(
    .listafy = function(df) {
      mylist <- list()
      for (i in 1:nrow(df)) {
        cohortData <- list(
          cohortId = df$cohortId[i],
          cohortName = df$cohortName[i],
          cohortDefinition = df$json[i]
        )
        mylist[[i]] <- cohortData
      }
      return(mylist)
    }
  )
)
