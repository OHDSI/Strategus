# CohortGeneratorModule -------------
#' @title Generate cohorts with the \href{https://ohdsi.github.io/CohortGenerator/}{HADES CohortGenerator Package}
#' @export
#' @description
#' Generates cohorts against the OMOP Common Data Model
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
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      cohortDefinitionSet <- super$.createCohortDefinitionSetFromJobContext(
        generateStats = jobContext$settings$generateStats
      )
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
        databaseId = jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount,
        incremental = jobContext$moduleExecutionSettings$incremental,
        incrementalFolder = jobContext$moduleExecutionSettings$workSubFolder
      )

      private$.message(paste("Results available at:", resultsFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = "") {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      CohortGenerator::createResultsDataModel(
        connectionDetails = resultsConnectionDetails,
        databaseSchema = resultsDatabaseSchema,
        tablePrefix = tablePrefix
      )
    },
    #' @description Get the results data model specification for the module
    #' @template tablePrefix
    getResultsDataModelSpecification = function(tablePrefix = "") {
      resultsDataModelSpecification <- CohortGenerator::readCsv(
        file = system.file(
          file.path("csv", "resultsDataModelSpecification.csv"),
          package = "CohortGenerator"
        ),
        warnOnCaseMismatch = FALSE
      )

      # add the prefix to the tableName column
      resultsDataModelSpecification$tableName <- paste0(tablePrefix, resultsDataModelSpecification$tableName)
      return(resultsDataModelSpecification)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)

      CohortGenerator::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        resultsFolder = private$jobContext$moduleExecutionSettings$resultsSubFolder,
        purgeSiteDataBeforeUploading = FALSE
      )
    },
    #' @description Creates the CohortGenerator Module Specifications
    #' @param generateStats When TRUE, the Circe cohort definition SQL will
    #' include steps to compute inclusion rule statistics.
    createModuleSpecifications = function(generateStats = TRUE) {
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
      cohortDefinitionSetFiltered <-  private$.listafy(parentCohortDefinitionSet)
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
    #' @param moduleSpecifications The CohortGenerator module specifications
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
