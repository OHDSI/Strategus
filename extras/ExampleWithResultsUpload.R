library(dplyr)
library(CohortGenerator)
library(Strategus)
library(ROhdsiWebApi)

baseUrl <- "https://change.me:8443/WebAPI"

atlasCohortIds <- c(5903, 5904)

username <- "changeme" # Set to your atlas username
dbUsername <- username

if (.Platform$OS.type == "unix") {
  ROhdsiWebApi::authorizeWebApi(baseUrl = baseUrl,
                                webApiUsername = username,
                                webApiPassword = getPass::getPass(),
                                authMethod = "windows")
} else {
  ROhdsiWebApi::authorizeWebApi(baseUrl = baseUrl,
                                authMethod = "windows")
}

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(baseUrl = baseUrl,
                                                               cohortIds = atlasCohortIds,
                                                               generateStats = TRUE)

dir.create("tmp", showWarnings = F)
source("https://raw.githubusercontent.com/OHDSI/CohortDiagnosticsModule/0.0.8/SettingsFunctions.R")

cohortDiagnosticsModuleSpecifications <- createCohortDiagnosticsModuleSpecifications(
  cohortIds = atlasCohortIds,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  incremental = TRUE
)


# Create analysis specifications ---------------------------------------------

source("https://raw.githubusercontent.com/OHDSI/CohortGeneratorModule/main/SettingsFunctions.R")

cohortDefinitionSharedResource <- createCohortSharedResourceSpecifications(cohortDefinitionSet)
cohortGeneratorModuleSpecifications <- createCohortGeneratorModuleSpecifications(incremental = TRUE,
                                                                                 generateStats = TRUE)

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  addSharedResources(cohortDefinitionSharedResource) %>%
  addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications)


# Create execution settings -----------------------------------------------------
connectionDetailsReference <- "eunomia-perm"
Strategus::storeConnectionDetails(Eunomia::getEunomiaConnectionDetails(file.path(normalizePath("tmp"), "eunomia-perm.sqlite")), connectionDetailsReference)

resultsConnectionReference <- "result-store"
resultsConnectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sqlite", server = file.path(normalizePath("tmp"), "results.sqlite"))
Strategus::storeConnectionDetails(resultsConnectionDetails, resultsConnectionReference)

# Note: this environmental variable should be set once for each compute node
Sys.setenv("INSTANTIATED_MODULES_FOLDER" = "~/tmp/StrategusInstantiatedModules")

# This should be ran once and only once
resultsExecutitionSettings <- Strategus::createResultsExecutionSettings(resultsConnectionDetailsReference = "result-store",
                                                                        resultsDatabaseSchema = "main",
                                                                        workFolder = file.path(getwd(), "./tmp/strategusWork"),
                                                                        resultsFolder = file.path(getwd(),"./tmp/strategusOutput"))

# Create results schemas for all modules - this is only needed once
Strategus::createResultDataModels(analysisSpecifications = analysisSpecifications, executionSettings = resultsExecutitionSettings)

# Note: Need to do only once: store con
executionSettings <- Strategus::createCdmExecutionSettings(connectionDetailsReference = connectionDetailsReference,
                                                           workDatabaseSchema = "main",
                                                           cdmDatabaseSchema = "main",
                                                           cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "strategus_test"),
                                                           workFolder = file.path(getwd(), "./tmp/strategusWork"),
                                                           resultsFolder = file.path(getwd(),"./tmp/strategusOutput"),
                                                           minCellCount = 5,
                                                           resultsDatabaseSchema = "main",
                                                           resultsConnectionDetailsReference  = resultsConnectionReference)

ParallelLogger::saveSettingsToJson(executionSettings, "testExecutionSettings.json")

# Execute analyses -------------------------------------------------------------
unlink("_targets", recursive = TRUE)

Strategus::execute(analysisSpecifications = analysisSpecifications,
                   executionSettings = executionSettings)
