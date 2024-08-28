# CohortIncidenceModule -------------
#' @title Module for computing incidence rates for cohorts against an OMOP CDM
#' @export
#' @description
#' Computes incidence rates for cohorts against the OMOP CDM
CohortIncidenceModule <- R6::R6Class(
  classname = "CohortIncidenceModule",
  inherit = StrategusModule,
  public = list(
    #' @field tablePrefix The table prefix to append to results tables
    tablePrefix = "ci_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Execute the CohortIncidence package
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      refId <- 1 # this should be part of execution settings
      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder

      # Establish the connection and ensure the cleanup is performed
      connection <- DatabaseConnector::connect(connectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))

      # extract CohortIncidence design from jobContext
      irDesign <- CohortIncidence::IncidenceDesign$new(private$jobContext$settings$irDesign)
      irDesignJSON <- as.character(irDesign$asJSON())

      # construct buildOptions from executionSettings
      # Questions:
      # Will there be a subgroup cohort table?
      # Are we pulling the source name from the right place?

      buildOptions <- CohortIncidence::buildOptions(
        cohortTable = paste0(private$jobContext$moduleExecutionSettings$workDatabaseSchema, ".", private$jobContext$moduleExecutionSettings$cohortTableNames$cohortTable),
        cdmDatabaseSchema = private$jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        sourceName = as.character(jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId),
        refId = refId
      )

      executeResults <- CohortIncidence::executeAnalysis(
        connection = connection,
        incidenceDesign = irDesignJSON,
        buildOptions = buildOptions
      )

      # Export the results
      exportFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      if (!dir.exists(exportFolder)) {
        dir.create(exportFolder, recursive = TRUE)
      }

      private$.message("Export data")

      # apply minCellCount to  executeResults
      minCellCount <- private$jobContext$moduleExecutionSettings$minCellCount
      if (minCellCount > 0) {
        executeResults$incidence_summary <- private$.enforceMinCellValue(executeResults$incidence_summary, "PERSONS_AT_RISK_PE", minCellCount)
        executeResults$incidence_summary <- private$.enforceMinCellValue(executeResults$incidence_summary, "PERSONS_AT_RISK", minCellCount)
        executeResults$incidence_summary <- private$.enforceMinCellValue(executeResults$incidence_summary, "PERSON_OUTCOMES_PE", minCellCount)
        executeResults$incidence_summary <- private$.enforceMinCellValue(executeResults$incidence_summary, "PERSON_OUTCOMES", minCellCount)
        executeResults$incidence_summary <- private$.enforceMinCellValue(executeResults$incidence_summary, "OUTCOMES_PE", minCellCount)
        executeResults$incidence_summary <- private$.enforceMinCellValue(executeResults$incidence_summary, "OUTCOMES", minCellCount)
        executeResults$incidence_summary <- private$.enforceMinCellStats(executeResults$incidence_summary)
      }

      for (tableName in names(executeResults)) {
        tableData <- executeResults[[tableName]]
        if (tableName == 'incidence_summary') {
          if (nrow(tableData) > 0) {
            tableData$database_id <- private$jobContext$moduleExecutionSettings$cdmDatabaseMetaData$databaseId
          } else {
            tableData$database_id <- character(0)
          }
        }
        readr::write_csv(tableData, file.path(exportFolder, paste0(self$tablePrefix,tableName,".csv")))
      }

      # in addition to the output of the module, we will produce a T-O lookup table that can be used to filter results
      # to either 'Outcomes for T' or 'Targets for Outcomes'

      targetOutcomeDfList <- lapply(irDesign$analysisList, function(analysis) {
        outcomeDefs <- Filter(function (o) o$id %in% analysis$outcomes, irDesign$outcomeDefs)
        outcome_cohort_id <- sapply(outcomeDefs, function(o) o$cohortId)
        as.data.frame(expand.grid(target_cohort_id = analysis$targets, outcome_cohort_id = outcome_cohort_id))
      })

      target_outcome_ref <- unique(do.call(rbind, targetOutcomeDfList))
      target_outcome_ref$ref_id <- refId
      readr::write_csv(target_outcome_ref, file.path(exportFolder, paste0(self$tablePrefix,"target_outcome_ref",".csv")))

      resultsDataModel <- private$.getResultsDataModelSpecification()
      readr::write_csv(resultsDataModel, file.path(exportFolder, "resultsDataModelSpecification.csv"))

      private$.message(paste("Results available at:", resultsFolder))
    },
    #' @description Create the results data model for the module
    #' @template resultsConnectionDetails
    #' @template resultsDatabaseSchema
    #' @template tablePrefix
    createResultsDataModel = function(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix = "") {
      super$createResultsDataModel(resultsConnectionDetails, resultsDatabaseSchema, tablePrefix)
      if (resultsConnectionDetails$dbms == "sqlite" & resultsDatabaseSchema != "main") {
        stop("Invalid schema for sqlite, use databaseSchema = 'main'")
      }

      connection <- DatabaseConnector::connect(resultsConnectionDetails)
      on.exit(DatabaseConnector::disconnect(connection))

      # Create the results model
      sql <- ResultModelManager::generateSqlSchema(schemaDefinition = private$.getResultsDataModelSpecification())
      sql <- SqlRender::render(sql= sql, warnOnMissingParameters = TRUE, database_schema = resultsDatabaseSchema)
      sql <- SqlRender::translate(sql = sql, targetDialect = resultsConnectionDetails$dbms)
      DatabaseConnector::executeSql(connection, sql)
    },
    #' @description Upload the results for the module
    #' @template resultsConnectionDetails
    #' @template analysisSpecifications
    #' @template resultsDataModelSettings
    uploadResults = function(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings) {
      super$uploadResults(resultsConnectionDetails, analysisSpecifications, resultsDataModelSettings)
      resultsFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder
      exportFolder <- private$jobContext$moduleExecutionSettings$resultsSubFolder

      # use the results model spec that was saved along with the results output, not the embedded model spec.
      resultsModelSpec <- readr::read_csv(
        file = file.path(file.path(exportFolder, "resultsDataModelSpecification.csv")),
        show_col_types = FALSE
      )

      ResultModelManager::uploadResults(
        connectionDetails = resultsConnectionDetails,
        schema = resultsDataModelSettings$resultsDatabaseSchema,
        resultsFolder = resultsFolder,
        purgeSiteDataBeforeUploading = FALSE,
        specifications = resultsModelSpec
      )
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
      irDesign <- CohortIncidence::IncidenceDesign$new(moduleSpecifications$settings$irDesign)
      designJson <- rJava::J("org.ohdsi.analysis.cohortincidence.design.CohortIncidence")$fromJson(as.character(irDesign$asJSON()))

      invisible(designJson)
    }
  ),
  private = list(
    .enforceMinCellValue = function(data, fieldName, minValues, silent = FALSE) {
      toCensor <- !is.na(data[, fieldName]) & data[, fieldName] < minValues & data[, fieldName] != 0
      if (!silent) {
        percent <- round(100 * sum(toCensor) / nrow(data), 1)
        private$.message(
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
    },
    .getResultsDataModelSpecification = function() {
      rdms <- CohortGenerator::readCsv(
        file = private$.getResultsDataModelSpecificationFileLocation()
      )
      rdms$tableName <-paste0(self$tablePrefix, rdms$tableName)
      return(rdms)
    },
    .getResultsDataModelSpecificationFileLocation = function() {
      return(system.file(
        file.path("csv", "cohortIncidenceRdms.csv"),
        package = "Strategus"
      ))
    }
  )
)
