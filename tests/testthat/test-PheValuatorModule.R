library(testthat)
library(Strategus)

# Helper to create a minimal valid PheValuator analysis list.
createMockPheValuatorAnalysis <- function(analysisId = 1) {
  list(
    analysisId = analysisId,
    phenotype = "foo",
    cohortsToEvaluate = list(),
    description = paste("Test analysis", analysisId),
    createEvaluationCohortArgs = list(xSpecCohortId = 1),
    testPhenotypeAlgorithmArgs = list(phenotypeCohortId = 2, cutPoints = c("EV"))
  )
}

# createModuleSpecifications -----------------------------------------------
test_that("createModuleSpecifications returns valid specification", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )

  expect_s3_class(spec, "ModuleSpecifications")
  expect_equal(spec$module, "PheValuatorModule")
  expect_equal(spec$settings$analysisName, "Main")
  expect_true(is.list(spec$settings$pheValuatorAnalysisList))
})

test_that("createModuleSpecifications stores analysisName", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    analysisName = "Sensitivity",
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )

  expect_equal(spec$settings$analysisName, "Sensitivity")
})

test_that("PheValuator referenced cohort validation includes all cohort roles", {
  pvModule <- PheValuatorModule$new()
  private <- pvModule$.__enclos_env__$private
  analysisList <- list(list(
    phenotype = "foo",
    cohortsToEvaluate = list(
      phenotypeCohortId = c(1, 5),
      washoutPeriod = c(0, 0),
      xSpecCohortId = 2,
      xSensCohortId = 3,
      prevalenceCohortId = 4
    )
  ))
  cds <- data.frame(
    cohortId = c(1, 2, 3, 4, 5),
    cohortName = paste0("cohort", c(1, 2, 3, 4, 5)),
    json = "{}",
    sql = "SELECT 1",
    stringsAsFactors = FALSE
  )

  expect_no_error(private$.validateReferencedCohorts(analysisList, cds))
  subsetCds <- private$.subsetCohortDefinitionSet(cds, analysisList)
  expect_setequal(subsetCds$cohortId, c(1, 2, 3, 4, 5))

  formattedCds <- private$.formatCohortDefinitionSetForPheValuator(subsetCds)
  expect_equal(colnames(formattedCds)[1:4], c("cohortId", "cohortName", "sql", "json"))
})

test_that("PheValuator referenced cohort validation reports missing cohort roles", {
  pvModule <- PheValuatorModule$new()
  private <- pvModule$.__enclos_env__$private
  analysisList <- list(list(
    phenotype = "foo",
    cohortsToEvaluate = list(
      phenotypeCohortId = 1,
      washoutPeriod = 0,
      xSpecCohortId = 2,
      xSensCohortId = 3,
      prevalenceCohortId = 4
    )
  ))
  cds <- data.frame(
    cohortId = c(1, 2),
    cohortName = c("phenotype", "xSpec"),
    json = "{}",
    sql = "SELECT 1",
    stringsAsFactors = FALSE
  )

  expect_error(
    private$.validateReferencedCohorts(analysisList, cds),
    "xSensCohortId.*3.*prevalenceCohortId.*4"
  )
})

test_that("createModuleSpecifications errors when pheValuatorAnalysisList is missing", {
  pvModule <- PheValuatorModule$new()
  expect_error(
    pvModule$createModuleSpecifications()
  )
})

# validateModuleSpecifications ---------------------------------------------
test_that("validateModuleSpecifications succeeds with valid spec", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
    pheValuatorAnalysisList = list(createMockPheValuatorAnalysis())
  )
  expect_no_error(pvModule$validateModuleSpecifications(spec))
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



# addPheValuatorModuleSpecifications ---------------------------------------
test_that("addPheValuatorModuleSpecifications adds module to analysis", {
  pvModule <- PheValuatorModule$new()
  spec <- pvModule$createModuleSpecifications(
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
