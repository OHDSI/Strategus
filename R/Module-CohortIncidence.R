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
    #' @description Execute the CohortIncidence package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      checkmate::assertClass(executionSettings, "CdmExecutionSettings")

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
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = "") {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      stop("NOT IMPLEMENTED")
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsUploadSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsUploadSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsUploadSettings)
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
