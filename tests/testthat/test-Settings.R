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
    resultsDatabaseSchema = "test",
    workFolder = tempfile(),
    resultsFolder = tempfile(),
    minCellCount = 5
  )

  expect_equal(class(executionSettings), c("ResultsExecutionSettings", "ExecutionSettings"))
})
