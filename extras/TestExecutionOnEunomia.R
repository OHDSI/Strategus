library(Strategus)
connectionDetails <- Eunomia::getEunomiaConnectionDetails()

analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
                         package = "Strategus"
  )
)
studyRootFolder <- file.path("extras", "EunomiaTestStudy")
workFolder <- file.path(studyRootFolder, "work_folder")
resultsFolder <- file.path(studyRootFolder, "results_folder")
if (!dir.exists(studyRootFolder)) {
  dir.create(studyRootFolder, recursive = TRUE)
}

# Execute the study ---------------------------
executionSettings <- createCdmExecutionSettings(
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "unit_test"),
  workFolder = workFolder,
  resultsFolder = resultsFolder,
  modulesToExecute = c("SelfControlledCaseSeriesModule")
)

ParallelLogger::saveSettingsToJson(
  object = executionSettings,
  file.path(studyRootFolder, "eunomiaExecutionSettings.json")
)

executionSettings <- ParallelLogger::loadSettingsFromJson(
  fileName = file.path(studyRootFolder, "eunomiaExecutionSettings.json")
)

executionSettings$databaseId <- 12346
sccsModule <- Strategus::SelfControlledCaseSeriesModule$new()

debugonce(sccsModule$execute)
sccsModule$execute(
  connectionDetails = connectionDetails,
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings
)

Strategus::execute(
  connectionDetails = connectionDetails,
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings
)
