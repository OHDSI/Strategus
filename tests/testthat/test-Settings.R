test_that("Test analysis specification creation", {
  cohortSharedResource <- list(id = 1)
  class(cohortSharedResource) <- c("CohortDefinitionSharedResources", "SharedResources")

  negativeControlOutcomeCohortSharedResource <- list(id = 1)
  class(negativeControlOutcomeCohortSharedResource) <- c("NegativeControlOutcomeSharedResources", "SharedResources")

  moduleSpecifications <- list(
    module = "CohortGeneratorModule",
    version = "0.0.16",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = list()
  )
  class(moduleSpecifications) <- c("CohortGeneratorModuleSpecifications", "ModuleSpecifications")

  analysisSpecification <- createEmptyAnalysisSpecificiations() %>%
    addSharedResources(cohortSharedResource) %>%
    addSharedResources(negativeControlOutcomeCohortSharedResource) %>%
    addModuleSpecifications(moduleSpecifications)

  expect_equal(length(analysisSpecification$sharedResources), 2)
  expect_equal(length(analysisSpecification$moduleSpecifications), 1)
})

test_that("Create results execution settings", {
  executionSettings <- createResultsExecutionSettings(
    resultsConnectionDetailsReference = "test",
    resultsDatabaseSchema = "test",
    workFolder = tempfile(),
    resultsFolder = tempfile(),
    minCellCount = 5
  )

  expect_equal(class(executionSettings), c("ResultsExecutionSettings", "ExecutionSettings"))
})

test_that("Get module list", {
  moduleList <- getModuleList()
  expect_true(nrow(moduleList) > 0)
})

test_that("Verify that unlocking keyring without password fails", {
  allKeyrings <- keyring::keyring_list()
  if (!keyringName %in% allKeyrings$keyring) {
    keyring::keyring_create(keyring = keyringName, password = keyringPassword)
  }
  # Lock the keyring
  keyring::keyring_lock(keyring = keyringName)

  # Remove STRATEGUS_KEYRING_PASSWORD in case it is already set
  Sys.unsetenv("STRATEGUS_KEYRING_PASSWORD")

  # Try to unlock and expect error
  expect_error(unlockKeyring(keyring = keyringName))
})

test_that("Store and retrieve connection details", {
  # Setup keyring for the test
  Sys.setenv("STRATEGUS_KEYRING_PASSWORD" = keyringPassword)
  createKeyringForUnitTest(selectedKeyring = keyringName, selectedKeyringPassword = keyringPassword)
  on.exit(deleteKeyringForUnitTest())

  for (i in 1:length(connectionDetailsList)) {
    connectionDetails <- connectionDetailsList[[i]]$connectionDetails
    dbms <- connectionDetailsList[[i]]$connectionDetails$dbms

    message("************* Store connection details for ", dbms, " *************")

    # Verify that the connection details are valid
    # by connecting to the DB
    conn <- DatabaseConnector::connect(
      connectionDetails
    )
    DatabaseConnector::disconnect(conn)

    # Store the connection details in keyring
    storeConnectionDetails(
      connectionDetails = connectionDetails,
      connectionDetailsReference = dbms,
      keyringName = keyringName
    )

    connectionDetailsFromKeyring <- retrieveConnectionDetails(
      connectionDetailsReference = dbms,
      keyringName = keyringName
    )

    # Verify that the connection details retrieved
    # allow for a connection to the DB
    connFromKeyring <- DatabaseConnector::connect(
      connectionDetailsFromKeyring
    )
    expect_silent(DatabaseConnector::disconnect(connFromKeyring))
  }
})

test_that("Retrieve connection details that do not exists throws informative error", {
  # Setup keyring for the test
  Sys.setenv("STRATEGUS_KEYRING_PASSWORD" = keyringPassword)
  createKeyringForUnitTest(selectedKeyring = keyringName, selectedKeyringPassword = keyringPassword)
  on.exit(deleteKeyringForUnitTest())
  expect_error(retrieveConnectionDetails(connectionDetailsReference = "does-not-exist", keyringName = keyringName))
})
