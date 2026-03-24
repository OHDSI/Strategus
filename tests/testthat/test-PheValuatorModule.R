library(testthat)
library(Strategus)

# Helper to create a minimal valid PheValuator analysis list.
# Uses plain lists to avoid requiring the PheValuator package at test time.
createMockPheValuatorAnalysis <- function(analysisId = 1) {
  list(
    analysisId = analysisId,
    description = paste("Test analysis", analysisId),
    createEvaluationCohortArgs = list(xSpecCohortId = 1),
    testPhenotypeAlgorithmArgs = list(phenotypeCohortId = 2, cutPoints = c("EV"))
  )
}

# createModuleSpecifications -----------------------------------------------
test_that("createModuleSpecifications returns valid specification", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    phenotype = "Type 2 Diabetes",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )

  expect_s3_class(spec, "ModuleSpecifications")
  expect_equal(spec$module, "PheValuatorModule")
  expect_equal(spec$settings$phenotype, "Type 2 Diabetes")
  expect_equal(spec$settings$analysisName, "Main")
  expect_true(is.list(spec$settings$pheValuatorAnalysisList))
  expect_true(is.null(spec$settings$cohortDefinitionSet))
})

test_that("createModuleSpecifications stores analysisName", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    analysisName = "Sensitivity",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )

  expect_equal(spec$settings$analysisName, "Sensitivity")
})

test_that("createModuleSpecifications stores cohortDefinitionSet as list", {
  pvModule <- PheValuatorModule$new()
  cds <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("xSpec", "Prevalence"),
    json = c("{}", "{}"),
    sql = c("SELECT 1", "SELECT 2"),
    stringsAsFactors = FALSE
  )

  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    cohortDefinitionSet = cds,
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )

  # cohortDefinitionSet is stored as a list-of-lists (one per row)
  expect_true(is.list(spec$settings$cohortDefinitionSet))
  expect_length(spec$settings$cohortDefinitionSet, 2)
})

test_that("createModuleSpecifications with empty cohortDefinitionSet stores NULL", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    cohortDefinitionSet = data.frame(),
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )

  expect_true(is.null(spec$settings$cohortDefinitionSet))
})

test_that("createModuleSpecifications errors when phenotype is missing", {
  pvModule <- PheValuatorModule$new()
  expect_error(
    pvModule$createModuleSpecifications(
      pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
    )
  )
})

test_that("createModuleSpecifications errors when pheValuatorAnalysisList is missing", {
  pvModule <- PheValuatorModule$new()
  expect_error(
    pvModule$createModuleSpecifications(
      phenotype = "T2DM"
    )
  )
})

# validateModuleSpecifications ---------------------------------------------
test_that("validateModuleSpecifications succeeds with valid spec", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )
  expect_no_error(pvModule$validateModuleSpecifications(spec))
})

test_that("validateModuleSpecifications errors when phenotype is empty", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )
  spec$settings$phenotype <- ""
  expect_error(pvModule$validateModuleSpecifications(spec))
})

test_that("validateModuleSpecifications errors when phenotype is not character", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )
  spec$settings$phenotype <- 123
  expect_error(pvModule$validateModuleSpecifications(spec))
})

test_that("validateModuleSpecifications errors when pheValuatorAnalysisList is empty", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )
  spec$settings$pheValuatorAnalysisList <- list()
  expect_error(pvModule$validateModuleSpecifications(spec))
})

# getResultsDataModelSpecification -----------------------------------------
test_that("getResultsDataModelSpecification returns expected structure", {
  pvModule <- PheValuatorModule$new()
  rdms <- pvModule$getResultsDataModelSpecification()

  expect_true(is.data.frame(rdms))
  expect_true(all(c("tableName", "columnName", "dataType", "isRequired", "primaryKey") %in% names(rdms)))

  # Check expected tables exist
  tableNames <- unique(rdms$tableName)
  expect_true(any(grepl("algorithm_performance_results", tableNames)))
  expect_true(any(grepl("cohort_definition_set", tableNames)))
})

test_that("getResultsDataModelSpecification applies table prefix", {
  pvModule <- PheValuatorModule$new()
  rdms <- pvModule$getResultsDataModelSpecification()
  expect_true(all(startsWith(rdms$tableName, "pv_")))
})

test_that("getResultsDataModelSpecification applies additional prefix", {
  pvModule <- PheValuatorModule$new()
  rdms <- pvModule$getResultsDataModelSpecification(tablePrefix = "study1_")
  expect_true(all(startsWith(rdms$tableName, "study1_pv_")))
})

# tablePrefix field --------------------------------------------------------
test_that("tablePrefix is set correctly", {
  pvModule <- PheValuatorModule$new()
  expect_equal(pvModule$tablePrefix, "pv_")
})

# execute input validation -------------------------------------------------
test_that("execute rejects ResultsExecutionSettings", {
  pvModule <- PheValuatorModule$new()

  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )

  analysisSpecifications <- createEmptyAnalysisSpecifications() |>
    addPheValuatorModuleSpecifications(spec)

  resultsExecutionSettings <- createResultsExecutionSettings(
    resultsDatabaseSchema = "main",
    workFolder = file.path(tempDir, "work"),
    resultsFolder = file.path(tempDir, "results")
  )

  expect_error(
    pvModule$execute(
      connectionDetails = connectionDetails,
      analysisSpecifications = analysisSpecifications,
      executionSettings = resultsExecutionSettings
    )
  )
})

# addPheValuatorModuleSpecifications ---------------------------------------
test_that("addPheValuatorModuleSpecifications adds module to analysis", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    phenotype = "T2DM",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )

  analysisSpecifications <- createEmptyAnalysisSpecifications() |>
    addPheValuatorModuleSpecifications(spec)

  expect_length(analysisSpecifications$moduleSpecifications, 1)
  expect_equal(analysisSpecifications$moduleSpecifications[[1]]$module, "PheValuatorModule")
})

test_that("addPheValuatorModuleSpecifications errors with wrong module spec", {
  cgModule <- CohortGeneratorModule$new()
  cgSpec <- cgModule$createModuleSpecifications()

  expect_error(
    createEmptyAnalysisSpecifications() |>
      addPheValuatorModuleSpecifications(cgSpec)
  )
})
