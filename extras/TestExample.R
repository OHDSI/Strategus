library(Strategus)
library(dplyr)


# Create analysis specifications ---------------------------------------------

# Note: maybe this function should live in the module?
createTestModule1Specifications <- function(dataSetName = "cars") {
  specifications <- list(module = "TestModule1",
                         version = "0.0.1",
                         settings = list(dataSetName = dataSetName))
  class(specifications) <- c("TestModule1Specifications", "ModuleSpecifications")
  return(specifications)
}

analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
  addModuleSpecifications(createTestModule1Specifications())

ParallelLogger::saveSettingsToJson(analysisSpecifications, "extras/testAnalysisSpecifications.json")


# Create execution settings -----------------------------------------------------
connectionDetailsReference <- "prod-1-mdcd"

# Note: Need to do only once: store connection details in keyring:
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))

storeConnectionDetails(connectionDetails = connectionDetails,
                       connectionDetailsReference = connectionDetailsReference)

executionSettings <- createCdmExecutionSettings(connectionDetailsReference = connectionDetailsReference,
                                                workDatabaseSchema = "scratch_mschuemi",
                                                cdmDatabaseSchema = "cdm_truven_mdcd_v1978",
                                                cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "strategus_test"),
                                                workFolder = "c:/temp/strategusWork",
                                                resultsFolder = "c:/temp/strategusOutput",
                                                minCellCount = 5)

ParallelLogger::saveSettingsToJson(executionSettings, "extras/testExecutionSettings.json")

# Execute analyses -------------------------------------------------------------

# Note: this environmental variable should be set once for each compute node
Sys.setenv("INSTANTIATED_MODULES_FOLDER" = "c:/temp/StrategusInstantiatedModules")
unlink("_targets", recursive = TRUE)

execute(analysisSpecifications = analysisSpecifications,
        executionSettings = executionSettings)
