#Install packages required for this script ---------
#install.packages("remotes")
#install.packages("ParallelLogger")
#remotes::install_github("OHDSI/Strategus")
#remotes::install_github("OHDSI/Eunomia")
#remotes::install_github("OHDSI/CohortGenerator")

#Run the Eunomia study ---------
# Set the folder & environment variable for module storage
moduleFolder <- Sys.getenv("INSTANTIATED_MODULES_FOLDER")
studyFolder <- "d:/temp/strategus/EunomiaTestStudy"


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

# Set the working directory to studyFolder
# and use relative paths to test
setwd(studyFolder)

# Execute the study ---------
analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  fileName = system.file("testdata/analysisSpecification.json",
                         package = "Strategus")
)

# Filter to to CI only
filteredAnalysisSpecifications <- analysisSpecifications
filteredAnalysisSpecifications$moduleSpecifications <- list(filteredAnalysisSpecifications$moduleSpecifications[[3]])

resultsExecutionSettings <- Strategus::createResultsExecutionSettings(
  resultsConnectionDetailsReference = "eunomia",
  resultsDatabaseSchema = "main",
  workFolder = file.path("schema_creation", "work_folder"),
  resultsFolder = file.path("schema_creation", "results_folder")
)

executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = "eunomia",
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(),
  workFolder = "work_folder",
  resultsFolder = "results_folder",
  minCellCount = 5#,
  #resultsConnectionDetailsReference = "eunomia",
  #resultsDatabaseSchema = "main"
)

ParallelLogger::saveSettingsToJson(
  object = executionSettings,
  file.path(studyFolder, "eunomiaExecutionSettings.json")
)

executionSettings <- ParallelLogger::loadSettingsFromJson(
  fileName = file.path(studyFolder, "eunomiaExecutionSettings.json")
)

Strategus::storeConnectionDetails(
  connectionDetails,
  "eunomia"
)

Strategus::createResultDataModels(
  analysisSpecifications = filteredAnalysisSpecifications,
  executionSettings = resultsExecutionSettings,
  enforceModuleDependencies = F
)

Strategus::execute(
  analysisSpecifications = filteredAnalysisSpecifications,
  executionSettings = executionSettings,
  executionScriptFolder = file.path(studyFolder, "script_folder"),
  enforceModuleDependencies = F
)
