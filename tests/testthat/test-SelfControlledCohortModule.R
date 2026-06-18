library(testthat)
library(Strategus)

test_that("SelfControlledCohortModule can be instantiated", {
  sccModule <- SelfControlledCohortModule$new()
  expect_s3_class(sccModule, "SelfControlledCohortModule")
  expect_equal(sccModule$tablePrefix, "scc_")
})

test_that("SelfControlledCohortModule - create module settings requires sccAnalysesSpecifications", {
  sccModule <- SelfControlledCohortModule$new()

  # Missing sccAnalysesSpecifications parameter should cause an error
  expect_error(
    sccModule$createModuleSpecifications()
  )
})

test_that("SelfControlledCohortModule - create module settings with empty list", {
  sccModule <- SelfControlledCohortModule$new()

  # Empty list should be accepted as valid input
  # (The actual validation happens in the SelfControlledCohort package)
  specs <- sccModule$createModuleSpecifications(
    sccAnalysesSpecifications = list()
  )
  expect_s3_class(specs, "SelfControlledCohortModuleSpecifications")
  expect_true("settings" %in% names(specs))
  expect_true("module" %in% names(specs))
})

test_that("SelfControlledCohortModule - module specifications have correct structure", {
  sccModule <- SelfControlledCohortModule$new()

  specs <- sccModule$createModuleSpecifications(
    sccAnalysesSpecifications = list()
  )

  # Verify the specification structure
  expect_s3_class(specs, "SelfControlledCohortModuleSpecifications")
  expect_true("settings" %in% names(specs))
  expect_true("module" %in% names(specs))
  expect_equal(specs$module, "SelfControlledCohortModule")
})

test_that("SelfControlledCohortModule - validateModuleSpecifications works", {
  sccModule <- SelfControlledCohortModule$new()

  specs <- sccModule$createModuleSpecifications(
    sccAnalysesSpecifications = list()
  )

  # Validation should complete without error for valid specs
  expect_no_error(
    sccModule$validateModuleSpecifications(
      moduleSpecifications = specs
    )
  )
})

test_that("SelfControlledCohortModule - getResultsDataModelSpecification returns data model", {
  sccModule <- SelfControlledCohortModule$new()

  # Get the data model specification
  dataModel <- sccModule$getResultsDataModelSpecification()

  # Verify it is a data frame with required columns
  expect_s3_class(dataModel, "data.frame")
  expect_true("tableName" %in% names(dataModel))
  expect_true("columnName" %in% names(dataModel))

  # Verify table names have the correct prefix
  expect_true(all(startsWith(unique(dataModel$tableName), "scc_")))
})

test_that("SelfControlledCohortModule - getResultsDataModelSpecification with custom prefix", {
  sccModule <- SelfControlledCohortModule$new()

  # Get the data model specification with a custom prefix
  dataModel <- sccModule$getResultsDataModelSpecification(tablePrefix = "custom_")

  # Verify it is a data frame
  expect_s3_class(dataModel, "data.frame")

  # Verify table names have the custom prefix combined with scc_ prefix
  expect_true(all(startsWith(unique(dataModel$tableName), "custom_scc_")))
})

