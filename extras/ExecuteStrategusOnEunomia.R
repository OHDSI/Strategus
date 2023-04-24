#Install packages required for this script ---------
#install.packages("remotes")
#install.packages("ParallelLogger")
#remotes::install_github("OHDSI/Strategus")
#remotes::install_github("OHDSI/Eunomia")
#remotes::install_github("OHDSI/CohortGenerator")

#Run the Eunomia study ---------
# Set the folder & environment variable for module storage
moduleFolder <- "C:/temp/strategus/modules"
studyFolder <- "C:/temp/strategus/EunomiaTestStudy"
Sys.setenv("INSTANTIATED_MODULES_FOLDER" = moduleFolder)

if (!dir.exists(moduleFolder)) {
  dir.create(moduleFolder, recursive = TRUE)
}
if (!dir.exists(studyFolder)) {
  dir.create(studyFolder, recursive = TRUE)
}

# Create the execution settings for Eunomia ----------
connectionDetails <- Eunomia::getEunomiaConnectionDetails(
  databaseFile = file.path(studyFolder, "cdm.sqlite")
)

Strategus::storeConnectionDetails(connectionDetails = connectionDetails,
                                  connectionDetailsReference = "eunomia")

executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = "eunomia",
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(),
  workFolder = file.path(studyFolder, "work_folder"),
  resultsFolder = file.path(studyFolder, "results_folder"),
  minCellCount = 5
)

ParallelLogger::saveSettingsToJson(
  object = executionSettings,
  file.path(studyFolder, "eunomiaExecutionSettings.json")
)

# Execute the study ---------
analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  fileName = system.file("testdata/analysisSpecification.json",
                         package = "Strategus")
)

executionSettings <- ParallelLogger::loadSettingsFromJson(
  fileName = file.path(studyFolder, "eunomiaExecutionSettings.json")
)

Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  executionScriptFolder = file.path(studyFolder, "script_folder")
)
