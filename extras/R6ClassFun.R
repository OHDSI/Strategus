library(Strategus)
library(CohortGenerator)

# Setup some test data ------------
cohortDefinitionSet <- getCohortDefinitionSet(
  settingsFileName = "testdata/Cohorts.csv",
  jsonFolder = "testdata/cohorts",
  sqlFolder = "testdata/sql",
  packageName = "Strategus"
)
ncoCohortSet <- readCsv(file = system.file("testdata/negative_controls_concept_set.csv",
                                           package = "Strategus"
))

# Create the job context manually for now - this will be
# something internal in Strategus later.
jc <- JobContext$new()
cgModuleSettingsCreator <- CohortGeneratorModuleSettings$new()

# Create the settings & validate them
cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

ncoCohortSharedResourceSpecifications <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSet, "first", TRUE)
cgModuleSettingsCreator$validateNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSharedResourceSpecifications)

cgModuleSettings <- cgModuleSettingsCreator$createModuleSpecifications()
cgModuleSettingsCreator$validateModuleSpecifications(cgModuleSettings)

jc$sharedResources <- list(
  cohortSharedResourcesSpecifications,
  ncoCohortSharedResourceSpecifications
)
jc$settings <- cgModuleSettings$settings #NOTE: This is done since the module settings are extracted from the larger analysis settings

# Cleanup any prior results
outputFolder <- "D:/TEMP/StrategusR6Testing"
unlink(outputFolder, recursive = T)
workFolder <- file.path(outputFolder, "work_folder")
resultsFolder <- file.path(outputFolder, "results_folder")
jc$moduleExecutionSettings <- Strategus::createCdmExecutionSettings(
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

cg <- CohortGeneratorModule$new(
  jobContext = jc,
  moduleIndex = 1,
  databaseId = "Eunomia"
)

cg$execute(
  connectionDetails = connectionDetails
)

#cg$createResultsSchema(NULL)
