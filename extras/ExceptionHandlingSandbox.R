# Create a simple class that throws an error upon execution
ExceptionThrowerModule <- R6::R6Class(
  classname = "ExceptionThrowerModule",
  inherit = Strategus:::StrategusModule,
  public = list(
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      checkmate::assertClass(executionSettings, "CdmExecutionSettings")
      private$errorFn("THROWING EXCEPTION")
    }
  ),
  private = list(
    errorFn = function(msg) {
      stop(msg)
    }
  )
)

# Create a simple class that prints a message
SimpleModule <- R6::R6Class(
  classname = "SimpleModule",
  inherit = Strategus:::StrategusModule,
  public = list(
    #' @description Initialize the module
    initialize = function() {
      super$initialize()
    },
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      checkmate::assertClass(executionSettings, "CdmExecutionSettings")
      private$.message("Hello World")
    }
  )
)


# Create a simple analysis spec with this module
etm <- ExceptionThrowerModule$new()
etmSpecs <- etm$createModuleSpecifications(list(foo = "bar"))

sm <- SimpleModule$new()
smSpecs <- sm$createModuleSpecifications(list(foo = "bar"))

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addModuleSpecifications(smSpecs) |>
  Strategus::addModuleSpecifications(etmSpecs) |>
  Strategus::addModuleSpecifications(smSpecs)

# Execute the study
executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "strategus_test"),
  workFolder = file.path(getwd(), "workFolder"),
  resultsFolder = file.path(getwd(), "resultsFolder"),
  minCellCount = 5
)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#debugonce(Strategus::execute)
executionStatus <- Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  connectionDetails = connectionDetails
)
