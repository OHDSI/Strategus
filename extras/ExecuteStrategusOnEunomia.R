#Install packages required for this script ---------
#install.packages("remotes")
#install.packages("ParallelLogger")
#remotes::install_github("OHDSI/Strategus")
#remotes::install_github("OHDSI/Eunomia")
#remotes::install_github("OHDSI/CohortGenerator")

#Run the Eunomia study ---------
# Set the folder & environment variable for module storage
moduleFolder <- "C:/temp/strategus/modules_dev"
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

# Set the working directory to studyFolder
# and use relative paths to test
setwd(studyFolder)

# Execute the study ---------
analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
  fileName = system.file("testdata/analysisSpecification.json",
                         package = "Strategus")
)

# Update to use the newer versions of modules
analysisSpecifications$moduleSpecifications[[1]]$version <- "0.2.2-7" # CG
analysisSpecifications$moduleSpecifications[[2]]$version <- "0.1.2-1" # CD
analysisSpecifications$moduleSpecifications[[3]]$version <- "0.3.1-1" # CI
analysisSpecifications$moduleSpecifications[[4]]$version <- "0.4.1-1" # CC
analysisSpecifications$moduleSpecifications[[5]]$version <- "0.2.2-1" # CM
analysisSpecifications$moduleSpecifications[[6]]$version <- "0.3.3-1" # SCCS
analysisSpecifications$moduleSpecifications[[7]]$version <- "0.2.2-1" # PLP

# Only CG
#analysisSpecifications$moduleSpecifications <- list(analysisSpecifications$moduleSpecifications[[1]])

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
  minCellCount = 5,
  resultsConnectionDetailsReference = "eunomia",
  resultsDatabaseSchema = "main"
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
  analysisSpecifications = analysisSpecifications,
  executionSettings = resultsExecutionSettings
)

Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  executionScriptFolder = file.path(studyFolder, "script_folder")
)
