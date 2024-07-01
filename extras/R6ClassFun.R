library(Strategus)
library(CohortGenerator)

# Setup some test data ------------
cohortDefinitionSet <- getCohortDefinitionSet(
  settingsFileName = "testdata/Cohorts.csv",
  jsonFolder = "testdata/cohorts",
  sqlFolder = "testdata/sql",
  packageName = "Strategus"
)
subsetOperations <- list(
  createDemographicSubset(
    name = "Demographic Criteria",
    ageMin = 18,
    ageMax = 64
  )
)
subsetDef <- createCohortSubsetDefinition(
  name = "test definition",
  definitionId = 1,
  subsetOperators = subsetOperations
)
cohortDefinitionSet <- cohortDefinitionSet |>
  addCohortSubsetDefinition(subsetDef)

ncoCohortSet <- readCsv(file = system.file("testdata/negative_controls_concept_set.csv",
                                           package = "Strategus"
))

# Create the analysis settings
cgModuleSettingsCreator <- CohortGeneratorModuleSettings$new()

# Create the settings & validate them
cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

ncoCohortSharedResourceSpecifications <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSet, "first", TRUE)
cgModuleSettingsCreator$validateNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSharedResourceSpecifications)

cgModuleSettings <- cgModuleSettingsCreator$createModuleSpecifications()
cgModuleSettingsCreator$validateModuleSpecifications(cgModuleSettings)

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortSharedResourcesSpecifications) |>
  addSharedResources(ncoCohortSharedResourceSpecifications) |>
  addModuleSpecifications(cgModuleSettings)

# Cleanup any prior results
outputFolder <- "D:/TEMP/StrategusR6Testing"
unlink(outputFolder, recursive = T)
dir.create(outputFolder, recursive = T)
ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path(outputFolder, "analysisSettings.json"))
workFolder <- file.path(outputFolder, "work_folder")
resultsFolder <- file.path(outputFolder, "results_folder")

executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = "eunomia", # TODO: This needs to go
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(),
  workFolder = workFolder,
  resultsFolder = resultsFolder,
  minCellCount = 5,
  resultsConnectionDetailsReference = "eunomia",
  resultsDatabaseSchema = "main"
)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#debugonce(Strategus::execute)
Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  connectionDetails = connectionDetails
)


# Can we treate a derived class like the base class?
foo <- get("JobContext")$new()
class(foo)
