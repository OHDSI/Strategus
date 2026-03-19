library(Strategus)
options("strategus.cohortSqlOptimizationFunction"= VaTools::translateToCustomVaSqlText)
tempDir <- tempfile()
tempDir <- gsub("\\\\", "/", tempDir) # Correct windows path

analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  fileName = system.file("testdata/cdmModulesAnalysisSpecifications.json",
                         package = "Strategus"
  )
)
studyRootFolder <- file.path(tempDir, "EunomiaTestStudy")
workFolder <- file.path(studyRootFolder, "work_folder")
resultsFolder <- file.path(studyRootFolder, "results_folder")
if (!dir.exists(studyRootFolder)) {
  dir.create(studyRootFolder, recursive = TRUE)
}

# Execute the study ---------------------------
executionSettings <- createCdmExecutionSettings(
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "test_cohort"),
  workFolder = workFolder,
  resultsFolder = resultsFolder,
  modulesToExecute = "CohortGeneratorModule"
)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()

Strategus::execute(
  connectionDetails = connectionDetails,
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings
)

unlink(studyRootFolder, recursive = TRUE, force = TRUE)

