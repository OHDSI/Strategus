# Job Context -------------
#' @title Job context holds the elements of the analysis specification
#' and execution settings necessary to execute a module.
#' @description
#' This is an internal class used by the StrategusModule (and child classes)
#' execute function
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
#' @title StrategusModule defines the base class for each HADES Strategus module
#' @export
#' @description
#' Provides a base class for HADES Strategus modules to inherit
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
      self$moduleName = class(self)[[1]]
      self$moduleClassName = paste0(self$moduleName, "Specifications")
    },
    #' @description Executes the module
    #' @param connectionDetails The connection details to the database
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param executionSettings The execution settings for the study
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
      checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
      checkmate::assertClass(executionSettings, "ExecutionSettings", add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      # Setup the job context
      private$.createJobContext(analysisSpecifications, executionSettings)
      # Setup logging
      private$.createLoggers(private$jobContext$moduleExecutionSettings)
      private$.message('EXECUTING: ', self$moduleName)
    },
    #' @description Create the results schema for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param resultsSchema The schema holding the results
    #' @param tablePrefix The prefix to use to append to the results tables (optional)
    createResultsSchema = function(resultsConnectionDetails, resultsSchema, tablePrefix = "") {
      private$.message('CREATE RESULTS SCHEMA: ', self$moduleName)
    },
    #' @description Upload the results for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param resultsExecutionSettings The results execution settings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings) {
      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertClass(resultsConnectionDetails, "ConnectionDetails", add = errorMessages)
      checkmate::assertClass(resultsExecutionSettings, "ResultsExecutionSettings", add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      # Setup the job context
      private$.createJobContext(analysisSpecifications, resultsExecutionSettings)
      private$.message('UPLOAD RESULTS: ', self$moduleName)
    },
    #' @description Base function for creating the module settings object.
    #' Each module will have its own implementation and this base class method
    #' will be used to ensure the class of the specifications is set properly.
    #' @param moduleSpecifications The module specifications
    createModuleSpecifications = function(moduleSpecifications) {
      moduleSpecifications = list(
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
    #' @param moduleSpecifications The module specifications
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
    .createLoggers = function(moduleExecutionSettings) {
      # TODO: Attaching these module-level loggers
      # seems to create an issue whereby only messages
      # emitted by the PL package are logged for some reason.
      # Come back to this..
      #
      # Establish loggers for the module execution
      # ParallelLogger::addDefaultFileLogger(
      #   name = "MODULE_LOGGER",
      #   fileName = file.path(moduleExecutionSettings$resultsSubFolder, "log.txt")
      # )
      # ParallelLogger::addDefaultErrorReportLogger(
      #   name = "MODULE_ERROR_LOGGER",
      #   file.path(moduleExecutionSettings$resultsSubFolder, "errorReport.R")
      # )
    },
    .clearLoggers = function() {
      #ParallelLogger::unregisterLogger("MODULE_LOGGER")
      #ParallelLogger::unregisterLogger("MODULE_ERROR_LOGGER")
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

      # Assemble the job context from the analysis specification
      # for the given module.
      private$jobContext$sharedResources <- analysisSpecifications$sharedResources
      private$jobContext$moduleExecutionSettings <- executionSettings
      private$jobContext$moduleExecutionSettings$workSubFolder <- file.path(private$jobContext$moduleExecutionSettings$workFolder, self$moduleName)
      private$jobContext$moduleExecutionSettings$resultsSubFolder <- file.path(private$jobContext$moduleExecutionSettings$resultsFolder, self$moduleName)

      # TODO: This should be in the execution settings already for
      # CDM ExecutionSettings
      #private$jobContext$moduleExecutionSettings$databaseId <- databaseId
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
    .jobContextHasNegativeControlOutcomeSharedResource = function() {
      jobContext <- private$jobContext
      ncSharedResource <- private$.getSharedResourceByClassName(
        sharedResources = jobContext$sharedResources,
        className = "NegativeControlOutcomeSharedResources"
      )
      hasNegativeControlOutcomeSharedResource <- !is.null(ncSharedResource)
      invisible(hasNegativeControlOutcomeSharedResource)
    },
    .createNegativeControlOutcomeSettingsFromJobContext = function() {
      jobContext <- private$jobContext
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
    #' @field cohortDefinitionSharedResourcesClassName A constant for the name
    #' of the cohort definition shared resources section of the analysis
    #' specification
    cohortDefinitionSharedResourcesClassName = "CohortDefinitionSharedResources",
    #' @field negativeControlOutcomeSharedResourcesClassName A constant for the
    #' name of the negative control outcome shared resources section of the
    #' analysis specification
    negativeControlOutcomeSharedResourcesClassName = "NegativeControlOutcomeSharedResources",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Generates the cohorts
    #' @param connectionDetails The connection details to the database
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param executionSettings The execution settings for the study
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      jobContext <- private$jobContext
      cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext()
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
        cohortDefinitionSet = cohortDefinitionSet,
        negativeControlOutcomeCohortSet = negativeControlOutcomeSettings$cohortSet,
        occurrenceType = negativeControlOutcomeSettings$occurrenceType,
        detectOnDescendants = negativeControlOutcomeSettings$detectOnDescendants,
        outputFolder = resultsFolder,
        databaseId = jobContext$moduleExecutionSettings$databaseId,
        incremental = jobContext$settings$incremental,
        incrementalFolder = jobContext$moduleExecutionSettings$workSubFolder
      )

      private$.message(paste("Results available at:", resultsFolder))
      private$.clearLoggers()
    },
    #' @description Create the results schema for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param resultsSchema The schema holding the results
    #' @param tablePrefix The prefix to use to append to the results tables (optional)
    createResultsSchema = function(resultsConnectionDetails, resultsSchema, tablePrefix = "") {
      super$createResultsSchema(resultsConnectionDetails, resultsSchema, tablePrefix)
      CohortGenerator::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsSchema,
        tablePrefix = tablePrefix
      )
    },
    #' @description Upload the results for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param resultsExecutionSettings The results execution settings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings)
      # TODO: The decisions to set the parameters:
      #    forceOverWriteOfSpecifications = FALSE
      #    purgeSiteDataBeforeUploading = FALSE
      # needs discussion.
      CohortGenerator::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsExecutionSettings$resultsDatabaseSchema,
        resultsFolder = private$jobContext$moduleExecutionSettings$resultsSubFolder,
        forceOverWriteOfSpecifications = FALSE,
        purgeSiteDataBeforeUploading = FALSE
      )
    },
    #' @description Creates the CohortGenerator Module Specifications
    #' @param incremental When TRUE, the module will keep track of the cohorts
    #' generated so that subsequent runs will skip any previously generated
    #' cohorts.
    #' @param generateStats When TRUE, the Circe cohort definition SQL will
    #' include steps to compute inclusion rule statistics.
    createModuleSpecifications = function(incremental = TRUE,
                                          generateStats = TRUE) {
      analysis <- list()
      for (name in names(formals(self$createModuleSpecifications))) {
        analysis[[name]] <- get(name)
      }

      specifications <- super$createModuleSpecifications(
        moduleSpecifications = analysis
      )
      return(specifications)
    },
    #' @description Create shared specifications for the cohort definition set
    #' @param cohortDefinitionSet The cohort definition set to include in the
    #' specification. See the CohortGenerator package for details on how to
    #' build this object.
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
        className = self$cohortDefinitionSharedResourcesClassName,
        sharedResourcesSpecifications = sharedResource
      )
      return(sharedResource)
    },
    #' @description Create shared specifications for the negative control outcomes cohort set
    #' @param negativeControlOutcomeCohortSet The negative control outcome cohort
    #' definition set defines the concepts to use to construct negative control
    #' outcome cohorts. See the CohortGenerator package for more details.
    #' @param occurrenceType Either "first" or "all
    #' @param detectOnDescendants When TRUE, the concept ID for the negative
    #' control will use the `concept_ancestor` table and will detect
    #' descendant concepts when constructing the cohort.
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
        className = self$negativeControlOutcomeSharedResourcesClassName,
        sharedResourcesSpecifications = sharedResource
      )
      return(sharedResource)
    },
    #' @description Validate the module specifications
    #' @param moduleSpecifications The module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    },
    #' @description Validate the cohort shared resource specifications
    #' @param cohortSharedResourceSpecifications The cohort shared resource specifications
    validateCohortSharedResourceSpecifications = function(cohortSharedResourceSpecifications) {
      super$validateSharedResourcesSpecifications(
        className = self$cohortDefinitionSharedResourcesClassName,
        sharedResourcesSpecifications = cohortSharedResourceSpecifications
      )
    },
    #' @description Validate the cohort shared resource specifications
    #' @param negativeControlOutcomeCohortSharedResourceSpecifications The cohort shared resource specifications
    validateNegativeControlOutcomeCohortSharedResourceSpecifications = function(negativeControlOutcomeCohortSharedResourceSpecifications) {
      super$validateSharedResourcesSpecifications(
        className = self$negativeControlOutcomeSharedResourcesClassName,
        sharedResourcesSpecifications = negativeControlOutcomeCohortSharedResourceSpecifications
      )
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

# CohortIncidenceModule -------------
#' @title Module for computing incidence rates for cohorts against an OMOP CDM
#' @export
#' @description
#' Computes incidence rates for cohorts against the OMOP CDM
CohortIncidenceModule <- R6::R6Class(
  classname = "CohortIncidenceModule",
  inherit = StrategusModule,
  public = list(
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the CohortIncidence packages
    #' @param connectionDetails The connection details to the database
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param executionSettings The execution settings for the study
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      jobContext <- private$jobContext
      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      private$.message("Validating inputs")
      private$.validate()

      # Establish the connection and ensure the cleanup is performed
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))

      # extract CohortIncidence design from jobContext
      irDesign <- as.character(CohortIncidence::IncidenceDesign$new(jobContext$settings$irDesign)$asJSON())


      # construct buildOptions from executionSettings
      # Questions:
      # Will there be a subgroup cohort table?
      # Are we pulling the source name from the right place?

      buildOptions <- CohortIncidence::buildOptions(
        cohortTable = paste0(jobContext$moduleExecutionSettings$workDatabaseSchema, ".", jobContext$moduleExecutionSettings$cohortTableNames$cohortTable),
        cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        sourceName = as.character(jobContext$moduleExecutionSettings$databaseId),
        refId = 1
      )

      executeResults <- CohortIncidence::executeAnalysis(
        connection = connection,
        incidenceDesign = irDesign,
        buildOptions = buildOptions
      )

      # Export the results
      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      if (!dir.exists(exportFolder)) {
        dir.create(exportFolder, recursive = TRUE)
      }

      rlang::inform("Export data")
      if (nrow(executeResults) > 0) {
        executeResults$database_id <- jobContext$moduleExecutionSettings$databaseId
      } else {
        executeResults$database_id <- character(0)
      }

      # apply minCellCount to  executeResults
      minCellCount <- jobContext$moduleExecutionSettings$minCellCount
      if (minCellCount > 0) {
        executeResults <- private$.enforceMinCellValue(executeResults, "PERSONS_AT_RISK_PE", minCellCount)
        executeResults <- private$.enforceMinCellValue(executeResults, "PERSONS_AT_RISK", minCellCount)
        executeResults <- private$.enforceMinCellValue(executeResults, "PERSON_OUTCOMES_PE", minCellCount)
        executeResults <- private$.enforceMinCellValue(executeResults, "PERSON_OUTCOMES", minCellCount)
        executeResults <- private$.enforceMinCellValue(executeResults, "OUTCOMES_PE", minCellCount)
        executeResults <- private$.enforceMinCellValue(executeResults, "OUTCOMES", minCellCount)
        executeResults <- private$.enforceMinCellStats(executeResults)
      }

      readr::write_csv(executeResults, file.path(exportFolder, "incidence_summary.csv")) # this will be renamed later

      # TODO: Move the results data model into the package
      # moduleInfo <- ParallelLogger::loadSettingsFromJson("MetaData.json")
      # resultsDataModel <- readr::read_csv(file = "resultsDataModelSpecification.csv", show_col_types = FALSE)
      # newTableNames <- paste0(moduleInfo$TablePrefix, resultsDataModel$"table_name")
      # # Rename export files based on table prefix
      # file.rename(
      #   file.path(exportFolder, paste0(unique(resultsDataModel$"table_name"), ".csv")),
      #   file.path(exportFolder, paste0(unique(newTableNames), ".csv"))
      # )
      # resultsDataModel$table_name <- newTableNames
      # readr::write_csv(resultsDataModel, file.path(exportFolder, "resultsDataModelSpecification.csv"))

      private$.message(paste("Results available at:", resultsFolder))
      private$.clearLoggers()
    },
    #' @description Create the results schema for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param resultsSchema The schema holding the results
    #' @param tablePrefix The prefix to use to append to the results tables (optional)
    createResultsSchema = function(resultsConnectionDetails, resultsSchema, tablePrefix = "") {
      super$createResultsSchema(resultsConnectionDetails, resultsSchema, tablePrefix)
      stop("NOT IMPLEMENTED")
    },
    #' @description Upload the results for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param resultsExecutionSettings The results execution settings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings)
      stop("NOT IMPLEMENTED")
    },
    #' @description Creates the CohortIncidence Module Specifications
    #' @param irDesign The incidence rate design created from the CohortIncidence
    #' package
    createModuleSpecifications = function(irDesign = NULL) {
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
    }
  ),
  private = list(
    .validate = function() {
      # Validate that the analysis specification will work when we
      # enter the execute statement. This is done by deserializing the design.
      irDesign <- CohortIncidence::IncidenceDesign$new(private$jobContext$settings$irDesign)
      designJson <- rJava::J("org.ohdsi.analysis.cohortincidence.design.CohortIncidence")$fromJson(as.character(irDesign$asJSON()))

      invisible(designJson)
    },
    .enforceMinCellValue = function(data, fieldName, minValues, silent = FALSE) {
      toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
      if (!silent) {
        percent <- round(100 * sum(toCensor) / nrow(data), 1)
        ParallelLogger::logInfo(
          "   censoring ",
          sum(toCensor),
          " values (",
          percent,
          "%) from ",
          fieldName,
          " because value below minimum"
        )
      }
      data[toCensor, fieldName] <- -minValues
      return(data)
    },
    .enforceMinCellStats = function(data) {
      # replace rates with NA for cencored outcomes
      toCensor <- data[, "OUTCOMES"] < 0
      data[toCensor, "INCIDENCE_RATE_P100PY"] <- NA

      # replace proportions with NA for censored person_outcomes
      toCensor <- data[, "PERSON_OUTCOMES"] < 0
      data[toCensor, "INCIDENCE_PROPORTION_P100P"] <- NA

      return(data)
    }
  )
)

# CohortMethodModule -------------
#' @title Module for performing new-user cohort studies
#' @export
#' @description
#' Module for performing new-user cohort studies in an observational
#' database in the OMOP Common Data Model.
CohortMethodModule <- R6::R6Class(
  classname = "CohortMethodModule",
  inherit = StrategusModule,
  public = list(
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Executes the CohortMethod packages
    #' @param connectionDetails The connection details to the database
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param executionSettings The execution settings for the study
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      jobContext <- private$jobContext

      multiThreadingSettings <- CohortMethod::createDefaultMultiThreadingSettings(parallel::detectCores())

      args <- jobContext$settings
      args$connectionDetails <- connectionDetails
      args$cdmDatabaseSchema <- jobContext$moduleExecutionSettings$cdmDatabaseSchema
      args$exposureDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$exposureTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outcomeDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema
      args$outcomeTable <- jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
      args$outputFolder <- jobContext$moduleExecutionSettings$workSubFolder
      args$multiThreadingSettings <- multiThreadingSettings
      args$cmDiagnosticThresholds <- NULL
      do.call(CohortMethod::runCmAnalyses, args)

      exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      CohortMethod::exportToCsv(
        outputFolder = jobContext$moduleExecutionSettings$workSubFolder,
        exportFolder = exportFolder,
        databaseId = jobContext$moduleExecutionSettings$databaseId,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        maxCores = parallel::detectCores(),
        cmDiagnosticThresholds = jobContext$settings$cmDiagnosticThresholds
      )
      # TODO: Removing this to make the upload easier
      #unlink(file.path(exportFolder, sprintf("Results_%s.zip", jobContext$moduleExecutionSettings$databaseId)))

      resultsDataModel <- CohortGenerator::readCsv(file = system.file("csv", "resultsDataModelSpecification.csv", package = "CohortMethod"))
      CohortGenerator::writeCsv(
        x = resultsDataModel,
        file = file.path(exportFolder, "resultsDataModelSpecification.csv"),
        warnOnFileNameCaseMismatch = FALSE
      )
      private$.message(paste("Results available at:", exportFolder))
      private$.clearLoggers()
    },
    #' @description Create the results schema for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param resultsSchema The schema holding the results
    #' @param tablePrefix The prefix to use to append to the results tables (optional)
    createResultsSchema = function(resultsConnectionDetails, resultsSchema, tablePrefix = "") {
      super$createResultsSchema(resultsConnectionDetails, resultsSchema, tablePrefix)
      CohortMethod::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsSchema,
        tablePrefix = tablePrefix
      )
    },
    #' @description Upload the results for the module
    #' @param resultsConnectionDetails The connection details to the results DB
    #' @param analysisSpecifications The analysis specifications for the study
    #' @param resultsExecutionSettings The results execution settings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsExecutionSettings)

      # TODO: This is something CM does differently.
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

      # TODO: The decisions to set the parameters:
      #    forceOverWriteOfSpecifications = FALSE
      #    purgeSiteDataBeforeUploading = FALSE
      # needs discussion.
      CohortMethod::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsExecutionSettings$resultsDatabaseSchema,
        zipFileName = zipFileName,
        forceOverWriteOfSpecifications = FALSE,
        purgeSiteDataBeforeUploading = FALSE
      )
    },
    #' @description Creates the CohortMethod Module Specifications
    #'
    #' @details
    #' Run a list of analyses for the target-comparator-outcomes of interest. This function will run all
    #' specified analyses against all hypotheses of interest, meaning that the total number of outcome
    #' models is `length(cmAnalysisList) * length(targetComparatorOutcomesList)` (if all analyses specify an
    #' outcome model should be fitted). When you provide several analyses it will determine whether any of
    #' the analyses have anything in common, and will take advantage of this fact. For example, if we
    #' specify several analyses that only differ in the way the outcome model is fitted, then this
    #' function will extract the data and fit the propensity model only once, and re-use this in all the
    #' analysis.
    #'
    #' After completion, a tibble containing references to all generated files can be obtained using the
    #' [getFileReference()] function. A summary of the analysis results can be obtained using the
    #' [getResultsSummary()] function.
    #'
    #' ## Analyses to Exclude
    #'
    #' Normally, `runCmAnalyses` will run all combinations of target-comparator-outcome-analyses settings.
    #' However, sometimes we may not need all those combinations. Using the `analysesToExclude` argument,
    #' we can remove certain items from the full matrix. This argument should be a data frame with at least
    #' one of the following columns:
    #'
    #' @param cmAnalysisList                 A list of objects of type `cmAnalysis` as created using
    #'                                       the `[CohortMethod::createCmAnalysis] function.
    #' @param targetComparatorOutcomesList   A list of objects of type `targetComparatorOutcomes` as
    #'                                       created using the [CohortMethod::createTargetComparatorOutcomes]
    #'                                       function.
    #' @param analysesToExclude              Analyses to exclude. See the Analyses to Exclude section for details.
    #' @param refitPsForEveryOutcome         Should the propensity model be fitted for every outcome (i.e.
    #'                                       after people who already had the outcome are removed)? If
    #'                                       false, a single propensity model will be fitted, and people
    #'                                       who had the outcome previously will be removed afterwards.
    #' @param refitPsForEveryStudyPopulation Should the propensity model be fitted for every study population
    #'                                       definition? If false, a single propensity model will be fitted,
    #'                                       and the study population criteria will be applied afterwards.
    #' @param cmDiagnosticThresholds An object of type `CmDiagnosticThresholds` as created using
    #'                                 [CohortMethod::createCmDiagnosticThresholds()].
    #'
    createModuleSpecifications = function(cmAnalysisList,
                                          targetComparatorOutcomesList,
                                          analysesToExclude = NULL,
                                          refitPsForEveryOutcome = FALSE,
                                          refitPsForEveryStudyPopulation = TRUE,
                                          cmDiagnosticThresholds = createCmDiagnosticThresholds()) {
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
    }
  )
)
