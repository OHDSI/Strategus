library(testthat)
library(Strategus)

test_that("Verify createModuleSpecification input validation works properly", {
  cModule <- CharacterizationModule$new()

  studyPopulationSettings <- Characterization::createStudyPopulationSettings(targetIds = 1)
  characterizationSettings <- Characterization::createCharacterizationSettings(
    timeToEventSettings = list(
      Characterization::createTimeToEventSettings(
        studyPopulationSettings = studyPopulationSettings,
        outcomeIds = 2
      )
    )
  )

  expect_silent(
    specs <- cModule$createModuleSpecifications(
      characterizationSettings = characterizationSettings
    )
  )
  expect_true(inherits(specs, "CharacterizationModuleSpecifications"))
  expect_identical(specs$settings$analysis, characterizationSettings)

  expect_error(
    cModule$createModuleSpecifications(
      targetIds = 1
    ),
    "Characterization v4 is required"
  )

  legacyArgs <- list(
    outcomeIds = 3,
    includeTimeToEvent = FALSE,
    includeTargetBaseline = FALSE,
    covariateSettings = list()
  )
  for (legacyArgName in names(legacyArgs)) {
    legacyCallArgs <- list(characterizationSettings = characterizationSettings)
    legacyCallArgs[[legacyArgName]] <- legacyArgs[[legacyArgName]]
    expect_error(
      do.call(cModule$createModuleSpecifications, legacyCallArgs),
      "legacy Characterization v3 arguments are no longer supported"
    )
  }

  specs <- cModule$createModuleSpecifications(
    characterizationSettings = characterizationSettings,
    minCharacterizationMean = 0.02,
    minCovariateCount = 10,
    minTargetSize = 100,
    minCaseSize = 50,
    mode = "Efficient",
    outputTable = "custom_characterization_cohorts"
  )
  expect_equal(specs$settings$minCharacterizationMean, 0.02)
  expect_equal(specs$settings$minCovariateCount, 10)
  expect_equal(specs$settings$minTargetSize, 100)
  expect_equal(specs$settings$minCaseSize, 50)
  expect_equal(specs$settings$mode, "Efficient")
  expect_equal(specs$settings$outputTable, "custom_characterization_cohorts")
})
