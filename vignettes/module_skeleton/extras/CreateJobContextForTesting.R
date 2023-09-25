# Create a job context for testing purposes
library(Strategus)
library(dplyr)
source("SettingsFunctions.R")

# Generic Helpers ----------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

# Sample Data Helpers ----------------------------
getSampleCohortDefintionSet <- function() {
  sampleCohorts <- CohortGenerator::createEmptyCohortDefinitionSet()
  cohortJsonFiles <- list.files(path = system.file("testdata/name/cohorts", package = "CohortGenerator"), full.names = TRUE)
  for (i in 1:length(cohortJsonFiles)) {
    cohortJsonFileName <- cohortJsonFiles[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(cohortJsonFileName)$size)
    sampleCohorts <- rbind(sampleCohorts, data.frame(
      cohortId = as.double(i),
      cohortName = cohortName,
      json = cohortJson,
      sql = "",
      stringsAsFactors = FALSE
    ))
  }

  # Add subsets to the cohort definition set
  maleOnlySubsetOperators <- list(
    CohortGenerator::createDemographicSubset(
      name = "Gender == Male",
      gender = 8507
    )
  )
  maleOnlySubsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "Males",
    definitionId = 1,
    subsetOperators = maleOnlySubsetOperators
  )
  # Define a subset for males age 40+
  maleAgeBoundedSubsetOperators <- list(
    CohortGenerator::createDemographicSubset(
      name = "Gender == Male, Age 40+",
      gender = 8507,
      ageMin = 40
    )
  )
  maleAgeBoundedSubsetDef <- CohortGenerator::createCohortSubsetDefinition(
    name = "Male, Age 40+",
    definitionId = 2,
    subsetOperators = maleAgeBoundedSubsetOperators
  )

  subsetDef1 <- CohortGenerator::createCohortSubsetDefinition(
    name = "Celecoxib new users, male >= 18, any exposure to celecoxib",
    definitionId = 3,
    subsetOperators = list(
      CohortGenerator::createCohortSubset(
        name = "Restrict to those with prior celecoxib",
        cohortIds = 1,
        negate = FALSE,
        cohortCombinationOperator = "all",
        startWindow = CohortGenerator::createSubsetCohortWindow(-99999, 99999, "cohortStart"),
        endWindow = CohortGenerator::createSubsetCohortWindow(-99999, 99999, "cohortStart")
      ),
      CohortGenerator::createLimitSubset(
        name = "Earlist event",
        priorTime = 365,
        followUpTime = 1,
        limitTo = "firstEver"
      ),
      CohortGenerator::createDemographicSubset(
        name = "Male and age 18+",
        ageMin = 18,
        gender = 8507
      )
    )
  )


  sampleCohorts <- sampleCohorts %>%
    CohortGenerator::addCohortSubsetDefinition(maleOnlySubsetDef) %>%
    CohortGenerator::addCohortSubsetDefinition(maleAgeBoundedSubsetDef) %>%
    CohortGenerator::addCohortSubsetDefinition(subsetDef1)
  return(sampleCohorts)
}

createCohortSharedResource <- function(cohortDefinitionSet = getSampleCohortDefintionSet()) {
  sharedResource <- createCohortSharedResourceSpecifications(cohortDefinitionSet = cohortDefinitionSet)
  return(sharedResource)
}

createNegativeControlSharedResource <- function() {
  negativeControlOutcomes <- readCsv(file = system.file("testdata/negativecontrols/negativecontrolOutcomes.csv",
    package = "CohortGenerator",
    mustWork = TRUE
  ))
  negativeControlOutcomes$cohortId <- negativeControlOutcomes$outcomeConceptId
  createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomes,
    occurrenceType = "all",
    detectOnDescendants = FALSE
  )
}

# Create CohortGeneratorModule settings ---------------------------------------
cohortGeneratorModuleSpecifications <- createCohortGeneratorModuleSpecifications(
  incremental = FALSE,
  generateStats = TRUE
)

# Module Settings Spec ----------------------------
analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
  addSharedResources(createCohortSharedResource()) %>%
  addSharedResources(createNegativeControlSharedResource()) %>%
  addModuleSpecifications(cohortGeneratorModuleSpecifications)

# executionSettings <- Strategus::createExecutionSettings(
executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = "dummy",
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
  workFolder = "dummy",
  resultsFolder = "dummy",
  minCellCount = 5
)

# Job Context ----------------------------
module <- "CohortGeneratorModule"
moduleIndex <- 1
moduleExecutionSettings <- executionSettings
moduleExecutionSettings$workSubFolder <- "dummy"
moduleExecutionSettings$resultsSubFolder <- "dummy"
moduleExecutionSettings$databaseId <- 123
jobContext <- list(
  sharedResources = analysisSpecifications$sharedResources,
  settings = analysisSpecifications$moduleSpecifications[[moduleIndex]]$settings,
  moduleExecutionSettings = moduleExecutionSettings
)
saveRDS(jobContext, "tests/testJobContext.rds")
# ParallelLogger::saveSettingsToJson(analysisSpecifications, fileName = "extras/analysisSettings.json")
