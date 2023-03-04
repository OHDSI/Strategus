library(dplyr)
library(CohortGenerator)
library(CohortDiagnostics)
library(ROhdsiWebApi)

baseUrl <- "https://change.me:8443/WebAPI"

atlasCohortIds <- c(5903, 5904)

username <- "jgilber2" # Set to your atlas username if it differs from your system username
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

subsetDef <- createCohortSubsetDefinition(
  name = "Without GI bleed",
  definitionId = 2,
  subsetOperators = list(
    # here we are saying 'first subset to only those patients in cohort 1778213'
    createCohortSubset(id = 1001,
                       name = "neg GI BLEED",
                       # Note that this can be set to any id - if the cohort is empty or doesn't exist this will not error
                       cohortIds = 5904,
                       cohortCombinationOperator = "any",
                       negate = TRUE,
                       startWindow = createSubsetCohortWindow(startDay = 0,
                                                              endDay = 30,
                                                              targetAnchor = "cohortStart"),
                       endWindow = createSubsetCohortWindow(startDay = 0,
                                                            endDay = 99999,
                                                            targetAnchor = "cohortStart"))
  )
)

subsetDef2 <- createCohortSubsetDefinition(
  name ="GI BLEED",
  definitionId = 1,
  subsetOperators = list(
    # here we are saying 'first subset to only those patients in cohort 1778213'
    createCohortSubset(id = 1001,
                       name = "GI BLEED",
                       # Note that this can be set to any id - if the cohort is empty or doesn't exist this will not error
                       cohortIds = 5904,
                       cohortCombinationOperator = "any",
                       negate = FALSE,
                       startWindow = createSubsetCohortWindow(startDay = 0,
                                                              endDay = 30,
                                                              targetAnchor = "cohortStart"),
                       endWindow = createSubsetCohortWindow(startDay = 0,
                                                            endDay = 99999,
                                                            targetAnchor = "cohortStart"))
  )
)

cohortDefinitionSet <- cohortDefinitionSet |>
  addCohortSubsetDefinition(subsetDef2, targetCohortIds = 5903, overwriteExisting = TRUE) |>
  addCohortSubsetDefinition(subsetDef, targetCohortIds = 5903, overwriteExisting = TRUE)


source("https://raw.githubusercontent.com/azimov/CohortDiagnosticsModule/main/SettingsFunctions.R")

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

source("https://raw.githubusercontent.com/OHDSI/CohortGeneratorModule/v0.0.16-3/SettingsFunctions.R")

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

# Note: this environmental variable should be set once for each compute node
Sys.setenv("INSTANTIATED_MODULES_FOLDER" = "~/tmp/StrategusInstantiatedModules")
unlink("_targets", recursive = TRUE)

Strategus::execute(analysisSpecifications = analysisSpecifications,
                   executionSettings = executionSettings)
