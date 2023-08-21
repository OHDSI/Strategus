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
testModule1Specifications <- createTestModule1Specifications()

# Module Settings Spec ----------------------------
analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
  addModuleSpecifications(testModule1Specifications)

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
module <- "TestModule1"
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

ParallelLogger::saveSettingsToJson(
  object = analysisSpecifications,
  fileName = "../../../inst/testdata/testModuleAnalysisSpecification.json"
)
