TreatmentPatternsModule <- R6::R6Class(
  classname = "TreatmentPatternsModule",
  inherit = strategusModule,
  public = list(
    #' @field tablePrefix The table prefix to append to the results tables
    tablePrefix = "c_",
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    #' @description Execute Treatment Patterns
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      # if(!is.null(executionSettings$cdmConn)){
      #   db <- DBI::dbConnect(odbc::odbc(),
      #                        driver = cdmExecutionSettings$cdmDriver,
      #                        server = cdmExecutionSettings$cdmDatabaseServer,
      #                        Uid = cdmExecutionSettings$user,
      #                        Pwd = cdmExecutionSettings$password,
      #                        port = cdmExecutionSettings$port,
      #                        Role = cdmExecutionSettings$role,
      #                        Database = cdmExecutionSettings$database,
      #                        Warehouse = cdmExecutionSettings$warehouse)
      #   cdm <- CDMConnector::cdm_from_con(con,
      #                                     cdm_schema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
      #                                     write_schema = jobContext$moduleExecutionSettings$workDatabaseSchema)
      # }

      TreatmentPatterns::executeTreatmentPatterns(
        cohorts = jobContext$settings,
        cohortTableName = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
        outputPath = resultsFolder,
        connectionDetails = cdmExecutionSettings$connectionDetails,
        cdmSchema =jobContext$moduleExecutionSettings$cdmDatabaseSchema,
        resultSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        tempEmulationSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
        minEraDuration = jobContext$moduleExecutionSettings$minEraDuration,
        eraCollapseSize = jobContext$moduleExecutionSettings$EraCollapse,
        combinationWindow = jobContext$moduleExecutionSettings$CombinationWindow,
        minCellCount = jobContext$moduleExecutionSettings$minCellCount

      )
    },
    #' @description Creates the CharacterizationModule Specifications
    #' @param
    #' @param cohorts
    #' @param cohortTableName
    #' @param cdm
    #' @param connectionDetails
    #' @param cdmSchema
    #' @param resultSchema
    #' @param tempEmulationSchema Schema used to emulate temp tables
    #' @param includeTreatments
    #' @param periodPriorToIndex
    #' @param minEraDuration
    #' @param splitEventCohorts
    #' @param splitTime
    #' @param eraCollapseSize
    #' @param combinationWindow
    #' @param minPostCombinationDuration
    #' @param filterTreatments
    #' @param maxPathLength
    createModuleSpecifications = function(cohorts,
                                          cohortTableName,
                                          cdm = NULL, # Not supporting
                                          connectionDetails = NULL,
                                          cdmSchema = NULL,
                                          resultSchema = NULL,
                                          tempEmulationSchema = NULL,
                                          includeTreatments = "startDate",
                                          periodPriorToIndex = 0,
                                          minEraDuration = 0,
                                          splitEventCohorts = NULL,
                                          splitTime = NULL,
                                          eraCollapseSize = 30,
                                          combinationWindow = 30,
                                          minPostCombinationDuration = 30,
                                          filterTreatments = "First",
                                          maxPathLength = 5
    ){
      # input checks
      validateComputePathways <- function() {
        args <- eval(
          # Expression to get names of function arguments in current function
          expr = expression(mget(names(formals()))),
          # Run expression in function that calls `validateComputePathways`
          envir = sys.frame(sys.nframe() - 1)
        )

        if (args$minEraDuration > args$minPostCombinationDuration) {
          warning("The `minPostCombinationDuration` is set lower than the `minEraDuration`, this might result in unexpected behavior")
        }

        if (args$minEraDuration > args$combinationWindow) {
          warning("The `combinationWindow` is set lower than the `minEraDuration`, this might result in unexpected behavior")
        }

        assertCol <- checkmate::makeAssertCollection()

        checkmate::assertCharacter(
          args$includeTreatments,
          len = 1,
          add = assertCol,
          .var.name = "includeTreatments"
        )

        checkmate::assertSubset(
          args$includeTreatments,
          choices = c("startDate", "endDate"),
          add = assertCol,
          .var.name = "includeTreatments"
        )

        checkmate::assertNumeric(
          args$periodPriorToIndex,
          len = 1,
          finite = TRUE,
          null.ok = FALSE,
          add = assertCol,
          .var.name = "periodPriorToIndex"
        )

        checkmate::assertNumeric(
          x = args$minEraDuration,
          lower = 0,
          finite = TRUE,
          len = 1,
          null.ok = FALSE,
          add = assertCol,
          .var.name = "minEraDuration"
        )

        checkmate::assertIntegerish(
          x = args$splitEventCohorts,
          null.ok = TRUE,
          add = assertCol,
          .var.name = "splitEventCohorts"
        )

        checkmate::assertIntegerish(
          x = args$splitTime,
          lower = 0,
          null.ok = TRUE,
          add = assertCol,
          .var.name = "splitTime"
        )

        checkmate::assertNumeric(
          x = args$eraCollapseSize,
          lower = 0,
          finite = TRUE,
          len = 1,
          null.ok = FALSE,
          add = assertCol,
          .var.name = "eraCollapseSize"
        )

        checkmate::assertNumeric(
          x = args$combinationWindow,
          lower = 0,
          finite = TRUE,
          len = 1,
          null.ok = FALSE,
          add = assertCol,
          .var.name = "combinationWindow"
        )

        checkmate::assertNumeric(
          x = args$minPostCombinationDuration,
          lower = 0,
          finite = TRUE,
          len = 1,
          null.ok = FALSE,
          add = assertCol,
          .var.name = "minPostCombinationDuration"
        )

        checkmate::assertCharacter(
          x = args$filterTreatments,
          len = 1,
          add = assertCol,
          .var.name = "filterTreatments"
        )

        checkmate::assertSubset(
          x = args$filterTreatments,
          choices = c("First", "Changes", "All"),
          add = assertCol,
          .var.name = "filterTreatments"
        )

        checkmate::assertNumeric(
          x = args$maxPathLength,
          lower = 0,
          upper = 5,
          finite = TRUE,
          len = 1,
          null.ok = FALSE,
          add = assertCol,
          .var.name = "maxPathLength"
        )

        checkmate::assertDataFrame(
          x = args$cohorts,
          types = c("integerish", "character", "character"),
          any.missing = FALSE,
          all.missing = FALSE,
          ncols = 3,
          min.rows = 1,
          col.names = "named",
          add = assertCol,
          .var.name = "cohorts"
        )

        checkmate::assertSubset(
          x = names(args$cohorts),
          choices = c("cohortId", "cohortName", "type"),
          add = assertCol,
          .var.name = "cohorts"
        )

        checkmate::assertSubset(
          x = args$cohorts$type,
          choices = c("event", "target", "exit"),
          add = assertCol,
          .var.name = "cohorts"
        )

        checkmate::assertCharacter(
          x = args$cohortTableName,
          len = 1,
          null.ok = FALSE,
          .var.name = "cohortTableName"
        )

        checkmate::assertClass(
          x = args$connectionDetails,
          classes = "ConnectionDetails",
          null.ok = TRUE,
          add = assertCol,
          .var.name = "connectionDetails"
        )

        checkmate::assertCharacter(
          x = args$connectionDetails$dbms,
          len = 1,
          null.ok = TRUE,
          add = assertCol,
          .var.name = "connectionDetails"
        )

        checkmate::assertCharacter(
          args$cdmDatabaseSchema,
          null.ok = TRUE,
          len = 1,
          add = assertCol,
          .var.name = "cdmDatabaseSchema"
        )

        checkmate::assertCharacter(
          args$resultSchema,
          null.ok = TRUE,
          len = 1,
          add = assertCol,
          .var.name = "resultSchema"
        )

        checkmate::assertClass(
          args$cdm,
          classes = "cdm_reference",
          null.ok = TRUE,
          add = assertCol,
          .var.name = "cdm"
        )

        checkmate::reportAssertions(collection = assertCol)
      }
      validateComputePathways()

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
    #' @param moduleSpecifications The CohortMethod module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
)
