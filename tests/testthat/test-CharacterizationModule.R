library(testthat)
library(Strategus)

test_that("Verify createModuleSpecification input validation works properly", {
  cModule <- CharacterizationModule$new()

  # outcomeIds must be a vector of numeric values
  expect_error(
    cModule$createModuleSpecifications(
      targetIds = 1,
      outcomeIds = c("incorrect_outcome")
    )
  )

  # An attempt to create module specifications where no analyses selected
  # returns an error
  expect_error(
    cModule$createModuleSpecifications(
      targetIds = 1,
      outcomeIds = 2,
      includeAggregateCovariate = FALSE,
      includeDechallengeRechallenge = FALSE,
      includeTimeToEvent = FALSE
    )
  )


  # Verify that excluding specific analyses returns NULL
  specs <- cModule$createModuleSpecifications(
    targetIds = 1,
    outcomeIds = 2,
    includeTimeToEvent = FALSE
  )
  expect_true(is.null(specs$settings$analysis$timeToEventSettings))

  specs <- cModule$createModuleSpecifications(
    targetIds = 1,
    outcomeIds = 2,
    includeDechallengeRechallenge = FALSE
  )
  expect_true(is.null(specs$settings$analysis$dechallengeRechallengeSettings))

  specs <- cModule$createModuleSpecifications(
    targetIds = 1,
    outcomeIds = 2,
    includeAggregateCovariate = FALSE
  )
  expect_true(is.null(specs$settings$analysis$aggregateCovariateSettings))


})
