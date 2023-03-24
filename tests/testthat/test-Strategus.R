test_that("Run Eunomia study", {
  moduleFolder <- file.path(tempDir, "strategus/modules")
  # NOTE: Need to set this in each test otherwise it goes out of scope
  renv:::renv_scope_envvars(RENV_PATHS_CACHE = renvCachePath)
  # Create a keyring called "strategus" that is password protected
  allKeyrings <- keyring::keyring_list()
  if (!"strategus" %in% allKeyrings$keyring) {
    keyring::keyring_create(keyring = "strategus", password = "foobar")
  }
  # Lock the keyring
  keyring::keyring_lock(keyring = "strategus")

  # Using a named and secured keyring
  Sys.setenv("STRATEGUS_KEYRING_PASSWORD" = "foobar")
  Strategus::storeConnectionDetails(
    connectionDetails = connectionDetails,
    connectionDetailsReference = dbms,
    keyringName = "strategus"
  )

  Sys.setenv("INSTANTIATED_MODULES_FOLDER" = moduleFolder)
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/analysisSpecification.json",
      package = "Strategus"
    )
  )

  # Use this line to limit to only running the CohortGeneratorModule
  # for testing purposes.
  analysisSpecifications$moduleSpecifications <- analysisSpecifications$moduleSpecifications[-c(2:length(analysisSpecifications$moduleSpecifications))]

  executionSettings <- createCdmExecutionSettings(
    connectionDetailsReference = dbms,
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
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

  # debugonce(Strategus::execute)
  Strategus::execute(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    executionScriptFolder = file.path(tempDir, "EunomiaTestStudy/script_folder"),
    keyringName = "strategus"
  )

  expect_true(file.exists(file.path(tempDir, "EunomiaTestStudy/results_folder/CohortGeneratorModule_1/done")))

  unlink(moduleFolder, recursive = TRUE, force = TRUE)
  unlink(file.path(tempDir, "EunomiaTestStudy"), recursive = TRUE, force = TRUE)
})
