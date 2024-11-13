# Job Context -------------
#' @title Job context holds the elements of the analysis specification
#' and execution settings necessary to execute a module.
#' @description
#' This is an internal class used by the StrategusModule (and child classes)
#' execute function
#' @noRd
#' @keywords internal
JobContext <- R6::R6Class(
  classname = "JobContext",
  public = list(
    #' @field sharedResources Shared resources for execution
    #' TODO: Revisit to break this into fields for cohorts, subsets,
    #' negative controls,
    sharedResources = list(),
    #' @field settings Module settings
    settings = list(),
    #' @field moduleExecutionSettings Module execution settings
    moduleExecutionSettings = list()
  )
)

# StrategusModule -------------
#' @title StrategusModule defines the base class for each HADES module
#' @export
#' @description
#' StrategusModule serves as an internal base class that defines the core
#' functions and structure to be inherited and implemented by any specific
#' HADES module. It provides a standardized framework for creating modular
#' components within the Strategus pipeline.
StrategusModule <- R6::R6Class(
  classname = "StrategusModule",
  public = list(
    #' @field moduleName The name of the module taken from the class name.
    #' This is set in the constructor of the class.
    moduleName = "",
    #' @field moduleClassName The class name that identifies
    #' the module specifications in the overall analysis specification.
    #' This is set in the constructor of the class.
    moduleClassName = "",
    #' @field internalModuleSpecificationClassName A constant value.
    #' The base class name that identifies a module specification
    #' in the analysis specification.
    internalModuleSpecificationClassName = "ModuleSpecifications",
    #' @field internalSharedResourcesClassName A constant value. The class name
    #' that identifies the shared resources section in the overall analysis
    #' specification.
    internalSharedResourcesClassName = "SharedResources",
    #' @description Initialize the module
    initialize = function() {
      self$moduleName <- class(self)[[1]]
      self$moduleClassName <- paste0(self$moduleName, "Specifications")
    },
    #' @description Executes the module
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
      checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
      checkmate::assertClass(executionSettings, "ExecutionSettings", add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      # Setup the job context
      private$.createJobContext(analysisSpecifications, executionSettings)
      private$.message("EXECUTING: ", self$moduleName)
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = "") {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails", add = errorMessages)
      checkmate::assertCharacter(resultsDatabaseSchema, len = 1, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)
      private$.message("CREATE RESULTS DATA MODEL: ", self$moduleName)
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails", add = errorMessages)
      checkmate::assertClass(resultsDataModelSettings, "ResultsDataModelSettings", add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      # Setup the job context
      private$.createJobContext(analysisSpecifications, resultsDataModelSettings)
      private$.message("UPLOAD RESULTS: ", self$moduleName)
    },
    #' @description Base function for creating the module settings object.
    #' Each module will have its own implementation and this base class method
    #' will be used to ensure the class of the specifications is set properly.
    #' @template moduleSpecifications
    createModuleSpecifications = function(moduleSpecifications) {
      moduleSpecifications <- list(
        module = self$moduleName,
        settings = moduleSpecifications
      )
      class(moduleSpecifications) <- c(self$internalModuleSpecificationClassName, self$moduleClassName)
      return(moduleSpecifications)
    },
    #' @description Base function for creating the shared resources settings object.
    #' Each module will have its own implementation if it needs to create
    #' a shared resource.
    #' @param className The class name of the shared resources specifications
    #' @param sharedResourcesSpecifications The shared resources specifications
    createSharedResourcesSpecifications = function(className, sharedResourcesSpecifications) {
      class(sharedResourcesSpecifications) <- c(className, self$internalSharedResourcesClassName)
      return(sharedResourcesSpecifications)
    },
    #' @description Base function for validating the module settings object.
    #' Each module will have its own implementation and this base class method
    #' will be used to ensure the module specifications are valid ahead of
    #' execution
    #' @template moduleSpecifications
    validateModuleSpecifications = function(moduleSpecifications) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(moduleSpecifications, self$internalModuleSpecificationClassName)
      checkmate::assertClass(moduleSpecifications, self$moduleClassName)
      checkmate::reportAssertions(collection = errorMessages)
    },
    #' @description Base function for validating the shared resources
    #' specification settings object. Each module will have its own
    #' implementation and this base class method will be used to ensure
    #' the module specifications are valid ahead of execution
    #' @param className The class name of the shared resources specifications
    #' @param sharedResourcesSpecifications The shared resources specifications
    validateSharedResourcesSpecifications = function(className, sharedResourcesSpecifications) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(sharedResourcesSpecifications, self$internalSharedResourcesClassName)
      checkmate::assertClass(sharedResourcesSpecifications, className)
      checkmate::reportAssertions(collection = errorMessages)
    }
  ),
  private = list(
    jobContext = JobContext$new(),
    .message = function(...) {
      rlang::inform(paste0(...))
    },
    .createJobContext = function(analysisSpecifications, executionSettings) {
      # Make sure this is created each call
      private$jobContext <- JobContext$new()
      # Get the moduleSpecification from the analysis specification
      # for the current class name.
      moduleSpecification <- private$.getModuleSpecification(
        analysisSpecifications = analysisSpecifications,
        moduleName = self$moduleName
      )
      if (is.null(moduleSpecification)) {
        stop(paste0(self$moduleName, " settings could not be found in the analysis specification."))
      }
      private$jobContext$settings <- moduleSpecification$settings

      # Make sure that the covariate settings for the analysis are updated
      # to reflect the location of the cohort tables
      private$jobContext$settings <- .replaceCovariateSettings(
        moduleSettings = private$jobContext$settings,
        executionSettings = executionSettings
      )

      # Assemble the job context from the analysis specification
      # for the given module.
      private$jobContext$sharedResources <- analysisSpecifications$sharedResources
      private$jobContext$moduleExecutionSettings <- executionSettings
      private$jobContext$moduleExecutionSettings$resultsSubFolder <- file.path(private$jobContext$moduleExecutionSettings$resultsFolder, self$moduleName)
      if (!dir.exists(private$jobContext$moduleExecutionSettings$resultsSubFolder)) {
        dir.create(private$jobContext$moduleExecutionSettings$resultsSubFolder, showWarnings = F, recursive = T)
      }

      if (is(private$jobContext$moduleExecutionSettings, "ExecutionSettings")) {
        private$jobContext$moduleExecutionSettings$workSubFolder <- file.path(private$jobContext$moduleExecutionSettings$workFolder, self$moduleName)
        if (!dir.exists(private$jobContext$moduleExecutionSettings$workSubFolder)) {
          dir.create(private$jobContext$moduleExecutionSettings$workSubFolder, showWarnings = F, recursive = T)
        }
      }
    },
    .getModuleSpecification = function(analysisSpecifications, moduleName) {
      moduleSpecification <- NULL
      for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
        curModuleName <- analysisSpecifications$moduleSpecifications[[i]]$module
        if (tolower(curModuleName) == tolower(moduleName)) {
          moduleSpecification <- analysisSpecifications$moduleSpecifications[[i]]
        }
      }
      return(moduleSpecification)
    },
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
    .createCohortDefinitionSetFromJobContext = function(generateStats) {
      jobContext <- private$jobContext
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
        generateStats = generateStats
      )
      return(cohortDefinitionSet)
    },
    .getCohortDefinitionSetFromSharedResource = function(cohortDefinitionSharedResource, generateStats) {
      cohortDefinitions <- cohortDefinitionSharedResource$cohortDefinitions
      if (length(cohortDefinitions) <= 0) {
        stop("No cohort definitions found")
      }
      cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()
      for (i in 1:length(cohortDefinitions)) {
        cohortJson <- cohortDefinitions[[i]]$cohortDefinition
        cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
        cohortSql <- CirceR::buildCohortQuery(cohortExpression, options = CirceR::createGenerateOptions(generateStats = generateStats))
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
    .createNegativeControlOutcomeSettingsFromJobContext = function() {
      jobContext <- private$jobContext
      negativeControlSharedResource <- private$.getSharedResourceByClassName(
        sharedResources = jobContext$sharedResources,
        className = "NegativeControlOutcomeSharedResources"
      )
      if (!is.null(negativeControlSharedResource)) {
        negativeControlOutcomes <- negativeControlSharedResource$negativeControlOutcomes$negativeControlOutcomeCohortSet
        if (length(negativeControlOutcomes) <= 0) {
          stop("Negative control outcome shared resource found but no negative control outcomes were provided.")
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
      } else {
        invisible(list(
          cohortSet = NULL,
          occurrenceType = "all",
          detectOnDescendants = FALSE
        ))
      }
    },
    .validateCdmExecutionSettings = function(executionSettings) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(executionSettings, "CdmExecutionSettings", add = errorMessages)
      checkmate::assertInt(executionSettings$maxCores, lower = 1, upper = parallel::detectCores())
    },
    .validateResultsExecutionSettings = function(executionSettings) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(executionSettings, "ResultsExecutionSettings", add = errorMessages)
      checkmate::assertInt(executionSettings$maxCores, lower = 1, upper = parallel::detectCores())
    }
  )
)

# Utility function to set the cohort table & schema on
# createCohortBasedCovariateSettings with information from
# the execution settings (Issue #181)
.replaceCovariateSettingsCohortTableNames <- function(covariateSettings, executionSettings) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(covariateSettings, min.len = 1, add = errorMessages)
  checkmate::assertClass(executionSettings, "ExecutionSettings", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  .replaceProperties <- function(s) {
    if (inherits(s, "covariateSettings") && "fun" %in% names(attributes(s))) {
      if (attr(s, "fun") == "getDbCohortBasedCovariatesData") {
        # Set the covariateCohortDatabaseSchema & covariateCohortTable values
        s$covariateCohortDatabaseSchema = executionSettings$workDatabaseSchema
        s$covariateCohortTable = executionSettings$cohortTableNames$cohortTable
      }
    }
    return(s)
  }
  if (is.null(names(covariateSettings))) {
    # List of lists
    modifiedCovariateSettings <- lapply(covariateSettings, .replaceProperties)
  } else {
    # Plain list
    modifiedCovariateSettings <- .replaceProperties(covariateSettings)
  }
  return(modifiedCovariateSettings)
}

.replaceCovariateSettings <- function(moduleSettings, executionSettings) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertList(moduleSettings, min.len = 1, add = errorMessages)
  checkmate::assertClass(executionSettings, "ExecutionSettings", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # A helper function to perform the replacement
  replaceHelper <- function(x) {
    if (is.list(x) && inherits(x, "covariateSettings")) {
      # If the element is a list and of type covariate settings
      # replace the cohort table names
      return(.replaceCovariateSettingsCohortTableNames(x, executionSettings))
    } else if (is.list(x)) {
      # If the element is a list, recurse on each element
      # Keep the original attributes by saving them before modification
      attrs <- attributes(x)
      newList <- lapply(x, replaceHelper)
      # Restore attributes to the new list
      attributes(newList) <- attrs
      return(newList)
    } else {
      # If the element is not a list or "covariateSettings", return it as is
      return(x)
    }
  }

  # Call the helper function on the input list
  return(replaceHelper(moduleSettings))
}


