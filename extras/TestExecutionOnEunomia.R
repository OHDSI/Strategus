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
if (dir.exists(studyRootFolder)) {
  unlink(studyRootFolder, recursive = TRUE, force = TRUE)
}
dir.create(studyRootFolder, recursive = TRUE)

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

Strategus::execute(
  connectionDetails = connectionDetails,
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings
)
