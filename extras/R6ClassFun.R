library(Strategus)

#m <- StrategusModule$new()
jc <- JobContext$new()

#m$execute(jc)
#m$createResultsSchema(NULL)

# Create the job context
library(CohortGenerator)
cohortDefinitionSet <- getCohortDefinitionSet(
  settingsFileName = "testdata/Cohorts.csv",
  jsonFolder = "testdata/cohorts",
  sqlFolder = "testdata/sql",
  packageName = "Strategus"
)
ncoCohortSet <- readCsv(file = system.file("testdata/negative_controls_concept_set.csv",
                                           package = "Strategus"
))

# Create the job context manually for the cohort generator settings
cgModuleSettings <- CohortGeneratorModuleSettings$new()
jc$sharedResources <- list(
  cgModuleSettings$createCohortSharedResourceSpecifications(cohortDefinitionSet),
  cgModuleSettings$createNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSet, "first", TRUE)
)
jc$settings <- cgModuleSettings$createModuleSpecifications()$settings
jc$moduleExecutionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = "eunomia", # TODO: This needs to go
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(),
  workFolder = "work_folder",
  resultsFolder = "results_folder",
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
