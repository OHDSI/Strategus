library(Strategus)
library(dplyr)


# Create analysis specifications ---------------------------------------------

# Note: maybe this function should live in the module?
createCohortGeneratorModuleSpecifications <- function() {
  specifications <- list(module = "CohortGeneratorModule",
                         version = "v0.0.4",
                         remoteRepo = "github.com",
                         remoteUsername = "anthonysena",
                         settings = list(incremental = TRUE,
                                         generateStats = TRUE))
  class(specifications) <- c("CohortGeneratorModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}

getSampleCohortDefintionSet <- function() {
  sampleCohorts <- CohortGenerator::createEmptyCohortDefinitionSet()
  cohortJsonFiles <- list.files(path = system.file("testdata/name/cohorts", package = "CohortGenerator"), full.names = TRUE)
  for (i in 1:length(cohortJsonFiles)) {
    cohortJsonFileName <- cohortJsonFiles[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
    sampleCohorts <- rbind(sampleCohorts, data.frame(cohortId = i,
                                                     cohortName = cohortName,
                                                     cohortDefinition = cohortJson,
                                                     stringsAsFactors = FALSE))
  }
  sampleCohorts <- apply(sampleCohorts,1,as.list)
  return(sampleCohorts)
}

createCohortSharedResource <- function(cohortDefinitionSet) {
  # Fill the cohort set using  cohorts included in this
  # package as an example
  sharedResource <- list(cohortDefinitions = cohortDefinitionSet)
  class(sharedResource) <- c("CohortDefinitionSharedResources", "SharedResources")
  return(sharedResource)
}

analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
  addSharedResources(createCohortSharedResource(getSampleCohortDefintionSet())) %>%
  addModuleSpecifications(createCohortGeneratorModuleSpecifications())

ParallelLogger::saveSettingsToJson(analysisSpecifications, "extras/cohortGeneratorAnalysisSpecifications.json")


# Create execution settings -----------------------------------------------------
connectionDetailsReference <- "Eunomia"

# Note: Need to do only once: store connection details in keyring:
connectionDetails <- Eunomia::getEunomiaConnectionDetails()

storeConnectionDetails(connectionDetails = connectionDetails,
                       connectionDetailsReference = connectionDetailsReference)

executionSettings <- createExecutionSettings(connectionDetailsReference = connectionDetailsReference,
                                             workDatabaseSchema = "main",
                                             cdmDatabaseSchema = "main",
                                             cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "strategus_cg_test"),
                                             workFolder = "c:/temp/strategusWork",
                                             resultsFolder = "c:/temp/strategusOutput",
                                             minCellCount = 5)

ParallelLogger::saveSettingsToJson(executionSettings, "extras/cohortGeneratorExecutionSettings.json")

# Execute analyses -------------------------------------------------------------

# Note: this environmental variable should be set once for each compute node
Sys.setenv("INSTANTIATED_MODULES_FOLDER" = "c:/temp/StrategusInstantiatedModules")

# Clean up before running
# unlink("_targets", recursive = TRUE)
# unlink(x = "c:/temp/StrategusInstantiatedModules", recursive = TRUE)
# unlink(x = "c:/temp/strategusWork", recursive = TRUE)
# unlink(x = "c:/temp/strategusOutput", recursive = TRUE)

execute(analysisSpecifications = analysisSpecifications,
        executionSettings = executionSettings,
        executionFolder = "c:/temp/strategusExecution")
