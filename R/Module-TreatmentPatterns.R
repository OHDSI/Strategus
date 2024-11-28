# TreatmentPatternsModule -------------
#' @title Evaluate phenotypes with the \href{https://github.com/darwin-eu/TreatmentPatterns/}{DARWIN TreatmentPatterns Package}
#' @export
#' @description
#' Characterisation and description of patterns of events (cohorts). against the OMOP Common Data Model.
TreatmentPatternsModule <- R6::R6Class(
  classname = "TreatmentPatternsModule",
  inherit = StrategusModule,

  ## Public ----
  public = list(
    ### Fields ----
    #' @field tablePrefix The table prefix to append to the results tables
    tablePrefix = "tp_",

    ### Methods ----
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },

    #' @description Execute Treatment Patterns
    #'
    #' @template connectionDetails
    #' @template analysisSpecifications
    #' @template executionSettings
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$.validateCdmExecutionSettings(executionSettings)
      super$execute(connectionDetails, analysisSpecifications, executionSettings)

      jobContext <- private$jobContext
      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder

      spec <- jobContext$settings
      outputEnv <- TreatmentPatterns::computePathways(
        cohorts = spec$cohorts,
        cohortTableName = spec$cohortTableName,
        connectionDetails = connectionDetails,
        cdmSchema = executionSettings$cdmDatabaseSchema,
        resultSchema = executionSettings$workDatabaseSchema,
        tempEmulationSchema = executionSettings$tempEmulationSchema,
        includeTreatments = spec$includeTreatments,
        indexDateOffset = spec$indexDateOffset,
        minEraDuration = spec$minEraDuration,
        splitEventCohorts = spec$splitEventCohorts,
        splitTime = spec$splitTime,
        eraCollapseSize = spec$eraCollapseSize,
        combinationWindow = spec$combinationWindow,
        minPostCombinationDuration = spec$minPostCombinationDuration,
        filterTreatments = spec$filterTreatments,
        maxPathLength = spec$maxPathLength
      )

      if (!dir.exists(executionSettings$resultsFolder)) dir.create(executionSettings$resultsFolder, recursive = TRUE, showWarnings = FALSE)

      TreatmentPatterns::export(
        andromeda = outputEnv,
        outputPath = executionSettings$resultsFolder,
        ageWindow = spec$ageWindow,
        minCellCount = executionSettings$minCellCount,
        censorType = spec$censorType,
        archiveName = NULL
      )

      on.exit(Andromeda::close(outputEnv))
    },

    #' @description Creates the TreatmentPatternsnModule Specifications
    #'
    #' @param cohorts (`data.frame()`)\cr
    #' Data frame containing the following columns and data types:
    #' \describe{
    #'  \item{cohortId `numeric(1)`}{Cohort ID's of the cohorts to be used in the cohort table.}
    #'  \item{cohortName `character(1)`}{Cohort names of the cohorts to be used in the cohort table.}
    #'  \item{type `character(1)` \["target", "event', "exit"\]}{Cohort type, describing if the cohort is a target, event, or exit cohort}
    #' }
    #' @param cohortTableName (`character(1)`)\cr
    #' Cohort table name.
    #' @param connectionDetails (`DatabaseConnector::createConnectionDetails()`: `NULL`)\cr
    #' Optional; In congruence with `cdmSchema` and `resultSchema`. Ignores `cdm`.
    #' @param cdmSchema (`character(1)`: `NULL`)\cr
    #' Optional; In congruence with `connectionDetails` and `resultSchema`. Ignores `cdm`.
    #' @param resultSchema (`character(1)`: `NULL`)\cr
    #' Optional; In congruence with `connectionDetails` and `cdmSchema`. Ignores `cdm`.
    #' @param tempEmulationSchema Schema used to emulate temp tables
    #' @param includeTreatments (`character(1)`: `"startDate"`)\cr
    #' \describe{
    #'  \item{`"startDate"`}{Include treatments after the target cohort start date and onwards.}
    #'  \item{`"endDate"`}{Include treatments before target cohort end date and before.}
    #' }
    #' @param indexDateOffset (`integer(1)`: `0`)\cr
    #' Offset the index date of the `Target` cohort.
    #' @param minEraDuration (`integer(1)`: `0`)\cr
    #' Minimum time an event era should last to be included in analysis
    #' @param splitEventCohorts (`character(n)`: `""`)\cr
    #' Specify event cohort to split in acute (< X days) and therapy (>= X days)
    #' @param splitTime (`integer(1)`: `30`)\cr
    #' Specify number of days (X) at which each of the split event cohorts should
    #' be split in acute and therapy
    #' @param eraCollapseSize (`integer(1)`: `30`)\cr
    #' Window of time between which two eras of the same event cohort are collapsed
    #' into one era
    #' @param combinationWindow (`integer(1)`: `30`)\cr
    #' Window of time two event cohorts need to overlap to be considered a
    #' combination treatment
    #' @param minPostCombinationDuration (`integer(1)`: `30`)\cr
    #' Minimum time an event era before or after a generated combination treatment
    #' should last to be included in analysis
    #' @param filterTreatments (`character(1)`: `"First"` \["first", "Changes", "all"\])\cr
    #' Select first occurrence of (‘First’); changes between (‘Changes’); or all
    #' event cohorts (‘All’).
    #' @param maxPathLength (`integer(1)`: `5`)\cr
    #' Maximum number of steps included in treatment pathway
    #' @param ageWindow (`integer(n)`: `10`)\cr
    #' Number of years to bin age groups into. It may also be a vector of integers.
    #' I.e. `c(0, 18, 150)` which will results in age group `0-18` which includes
    #' subjects `< 19`. And age group `18-150` which includes subjects `> 18`.
    #' @param minCellCount (`integer(1)`: `5`)\cr
    #' Minimum count required per pathway. Censors data below `x` as `<x`. This
    #' minimum value will carry over to the sankey diagram and sunburst plot.
    #' @param censorType (`character(1)`)\cr
    #' \describe{
    #'   \item{`"minCellCount"`}{Censors pathways <`minCellCount` to `minCellCount`.}
    #'   \item{`"remove"`}{Censors pathways <`minCellCount` by removing them completely.}
    #'   \item{`"mean"`}{Censors pathways <`minCellCount` to the mean of all frequencies below `minCellCount`}
    #' }
    createModuleSpecifications = function(cohorts,
                                          cohortTableName,
                                          connectionDetails = NULL,
                                          cdmSchema = NULL,
                                          resultSchema = NULL,
                                          tempEmulationSchema = NULL,
                                          includeTreatments = "startDate",
                                          indexDateOffset = 0,
                                          minEraDuration = 0,
                                          splitEventCohorts = NULL,
                                          splitTime = NULL,
                                          eraCollapseSize = 30,
                                          combinationWindow = 30,
                                          minPostCombinationDuration = 30,
                                          filterTreatments = "First",
                                          maxPathLength = 5,
                                          ageWindow = 5,
                                          minCellCount = 1,
                                          censorType = "minCellCount"
    ) {
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
          args$indexDateOffset,
          len = 1,
          finite = TRUE,
          null.ok = FALSE,
          add = assertCol,
          .var.name = "indexDateOffset"
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
    #'
    #' @param moduleSpecifications The CohortMethod module specifications
    validateModuleSpecifications = function(moduleSpecifications) {
      super$validateModuleSpecifications(
        moduleSpecifications = moduleSpecifications
      )
    }
  )
)
