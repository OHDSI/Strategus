
test_that("Verify createModuleSpecification input validation works properly", {
  cseModule <- ComparatorSelectionExplorerModule$new()

  # Ensure valid inputs do not raise errors
  specs <- cseModule$createModuleSpecifications(
    targetCohortIds = c(1, 2, 3),
    minExposureSize = 1000
  )
  expect_true(is.list(specs))
  expect_equal(specs$settings$targetCohortIds, c(1, 2, 3))
  expect_equal(specs$settings$minExposureSize, 1000)
})

test_that("Verify createModuleSpecifications handles optional parameters correctly", {
  cseModule <- ComparatorSelectionExplorerModule$new()

  # Verify that excluding optional parameters uses default values
  specs <- cseModule$createModuleSpecifications(
    targetCohortIds = c(1, 2, 3)
  )
  expect_equal(specs$settings$minExposureSize, 1000) # Default value

  # Verify that providing specific optional parameters overrides defaults
  specs <- cseModule$createModuleSpecifications(
    targetCohortIds = c(1, 2, 3),
    minExposureSize = 500,
    generateCohortDefinitionSet = FALSE
  )
  expect_equal(specs$settings$minExposureSize, 500)
})
