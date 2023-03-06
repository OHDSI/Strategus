test_that("Run Eunomia study", {
  # Skipping non-Windows environment testing for now since
  # configuring keyring on non-Windows environments was problematic
  # and also there are known issues with Strategus on non-Windows
  # devices: https://github.com/OHDSI/Strategus/issues/26
  skip_if_not_win()

  tempDir <- tempfile()
  tempDir <- gsub("\\\\", "/", tempDir) # Correct windows path
  renv:::renv_scope_envvars(RENV_PATHS_CACHE = tempDir)
  moduleFolder <- file.path(tempDir,"/strategus/modules")
  # Create a keyring called "strategus" that is password protected
  allKeyrings <- keyring::keyring_list()
  if (!"strategus" %in% allKeyrings$keyring) {
    keyring::keyring_create(keyring = "strategus", password = "foobar")
  }
  # Lock the keyring
  keyring::keyring_lock(keyring = "strategus")

  connectionDetails <- Eunomia::getEunomiaConnectionDetails()

  # Using a named and secured keyring
  Sys.setenv("STRATEGUS_KEYRING_PASSWORD"="foobar")
  Strategus::storeConnectionDetails(connectionDetails = connectionDetails,
                                    connectionDetailsReference = "eunomia",
                                    keyringName = "strategus")

  Sys.setenv("INSTANTIATED_MODULES_FOLDER" = moduleFolder)
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/analysisSpecification.json",
                           package = "Strategus")
  )

  # For now limit to CG
  analysisSpecifications$moduleSpecifications <- analysisSpecifications$moduleSpecifications[-c(2:length(analysisSpecifications$moduleSpecifications))]

  executionSettings <- createCdmExecutionSettings(
    connectionDetailsReference = "eunomia",
    workDatabaseSchema = "main",
    cdmDatabaseSchema = "main",
    cohortTableNames = CohortGenerator::getCohortTableNames(),
    workFolder = file.path(tempDir, "EunomiaTestStudy/work_folder"),
    resultsFolder = file.path(tempDir, "EunomiaTestStudy/results_folder"),
    minCellCount = 5
  )

  if (!dir.exists(file.path(tempDir, "EunomiaTestStudy"))) {
    dir.create(file.path(tempDir, "EunomiaTestStudy"), recursive = TRUE)
  }
  ParallelLogger::saveSettingsToJson(
    object = executionSettings,
    file.path(tempDir, "EunomiaTestStudy/eunomiaExecutionSettings.json")
  )

  executionSettings <- ParallelLogger::loadSettingsFromJson(
    fileName = file.path(tempDir, "EunomiaTestStudy/eunomiaExecutionSettings.json")
  )

  Strategus::execute(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    executionScriptFolder = file.path(tempDir, "EunomiaTestStudy/script_folder"),
    keyringName = "strategus"
  )

})
