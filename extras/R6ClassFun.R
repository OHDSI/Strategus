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

# Create the analysis settings ---------------

# Cohort Generator -----------------
cgModuleSettingsCreator <- CohortGeneratorModuleSettings$new()

# Create the settings & validate them
cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

ncoCohortSharedResourceSpecifications <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSet, "first", TRUE)
cgModuleSettingsCreator$validateNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSharedResourceSpecifications)

cgModuleSettings <- cgModuleSettingsCreator$createModuleSpecifications()
cgModuleSettingsCreator$validateModuleSpecifications(cgModuleSettings)

# Cohort Incidence -----------------
library(CohortIncidence)
ciModuleSettingsCreator <- CohortIncidenceModuleSettings$new()
targets <- list(
  createCohortRef(id = 1, name = "Celecoxib"),
  createCohortRef(id = 2, name = "Diclofenac"),
  createCohortRef(id = 4, name = "Celecoxib Age >= 30"),
  createCohortRef(id = 5, name = "Diclofenac Age >= 30")
)
outcomes <- list(createOutcomeDef(id = 1, name = "GI bleed", cohortId = 3, cleanWindow = 9999))

tars <- list(
  createTimeAtRiskDef(id = 1, startWith = "start", endWith = "end"),
  createTimeAtRiskDef(id = 2, startWith = "start", endWith = "start", endOffset = 365)
)
analysis1 <- createIncidenceAnalysis(
  targets = c(1, 2, 4, 5),
  outcomes = c(1),
  tars = c(1, 2)
)

irDesign <- createIncidenceDesign(
  targetDefs = targets,
  outcomeDefs = outcomes,
  tars = tars,
  analysisList = list(analysis1),
  strataSettings = createStrataSettings(
    byYear = TRUE,
    byGender = TRUE
  )
)

ciModuleSettings <- ciModuleSettingsCreator$createModuleSpecifications(
  irDesign = irDesign$toList()
)
ciModuleSettingsCreator$validateModuleSpecifications(ciModuleSettings)

analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortSharedResourcesSpecifications) |>
  addSharedResources(ncoCohortSharedResourceSpecifications) |>
  addModuleSpecifications(cgModuleSettings) |>
  addModuleSpecifications(ciModuleSettings)

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
