library(testthat)
library(Strategus)

test_that("EvidenceSynthesisModule can be instantiated", {
  esModule <- EvidenceSynthesisModule$new()
  expect_s3_class(esModule, "EvidenceSynthesisModule")
})

test_that("EvidenceSynthesisModule - createEvidenceSynthesisSource with default parameters", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource()

  expect_s3_class(source, "EvidenceSynthesisSource")
  expect_equal(source$sourceMethod, "CohortMethod")
  expect_null(source$databaseIds)
  expect_null(source$analysisIds)
  expect_equal(source$likelihoodApproximation, "grid with gradients")
})

test_that("EvidenceSynthesisModule - createEvidenceSynthesisSource with CohortMethod", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    databaseIds = c(1, 2, 3),
    analysisIds = c(1, 2),
    likelihoodApproximation = "normal"
  )

  expect_s3_class(source, "EvidenceSynthesisSource")
  expect_equal(source$sourceMethod, "CohortMethod")
  expect_equal(source$databaseIds, c(1, 2, 3))
  expect_equal(source$analysisIds, c(1, 2))
  expect_equal(source$likelihoodApproximation, "normal")
})

test_that("EvidenceSynthesisModule - createEvidenceSynthesisSource with SelfControlledCaseSeries", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "SelfControlledCaseSeries",
    likelihoodApproximation = "adaptive grid"
  )

  expect_s3_class(source, "EvidenceSynthesisSource")
  expect_equal(source$sourceMethod, "SelfControlledCaseSeries")
  expect_equal(source$likelihoodApproximation, "adaptive grid")
})

test_that("EvidenceSynthesisModule - createEvidenceSynthesisSource with SelfControlledCohort", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "SelfControlledCohort",
    likelihoodApproximation = "normal"
  )

  expect_s3_class(source, "EvidenceSynthesisSource")
  expect_equal(source$sourceMethod, "SelfControlledCohort")
  expect_equal(source$likelihoodApproximation, "normal")
})

test_that("EvidenceSynthesisModule - createEvidenceSynthesisSource rejects invalid sourceMethod", {
  esModule <- EvidenceSynthesisModule$new()

  expect_error(
    esModule$createEvidenceSynthesisSource(
      sourceMethod = "InvalidMethod"
    )
  )
})

test_that("EvidenceSynthesisModule - createEvidenceSynthesisSource rejects SCC with non-normal approximation", {
  esModule <- EvidenceSynthesisModule$new()

  expect_error(
    esModule$createEvidenceSynthesisSource(
      sourceMethod = "SelfControlledCohort",
      likelihoodApproximation = "adaptive grid"
    )
  )
})

test_that("EvidenceSynthesisModule - createRandomEffectsMetaAnalysis", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "normal"
  )

  analysis <- esModule$createRandomEffectsMetaAnalysis(
    alpha = 0.05,
    evidenceSynthesisAnalysisId = 1,
    evidenceSynthesisDescription = "Test Random Effects",
    evidenceSynthesisSource = source,
    controlType = "outcome"
  )

  expect_s3_class(analysis, "RandomEffectsMetaAnalysis")
  expect_s3_class(analysis, "EvidenceSynthesisAnalysis")
  expect_equal(analysis$alpha, 0.05)
  expect_equal(analysis$evidenceSynthesisAnalysisId, 1)
  expect_equal(analysis$evidenceSynthesisDescription, "Test Random Effects")
  expect_equal(analysis$controlType, "outcome")
})

test_that("EvidenceSynthesisModule - createRandomEffectsMetaAnalysis rejects non-normal approximation", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "adaptive grid"
  )

  expect_error(
    esModule$createRandomEffectsMetaAnalysis(
      evidenceSynthesisAnalysisId = 1,
      evidenceSynthesisSource = source
    )
  )
})

test_that("EvidenceSynthesisModule - createFixedEffectsMetaAnalysis", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "normal"
  )

  analysis <- esModule$createFixedEffectsMetaAnalysis(
    alpha = 0.05,
    evidenceSynthesisAnalysisId = 2,
    evidenceSynthesisDescription = "Test Fixed Effects",
    evidenceSynthesisSource = source,
    controlType = "outcome"
  )

  expect_s3_class(analysis, "FixedEffectsMetaAnalysis")
  expect_s3_class(analysis, "EvidenceSynthesisAnalysis")
  expect_equal(analysis$alpha, 0.05)
  expect_equal(analysis$evidenceSynthesisAnalysisId, 2)
})

test_that("EvidenceSynthesisModule - createFixedEffectsMetaAnalysis rejects non-normal approximation", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "grid with gradients"
  )

  expect_error(
    esModule$createFixedEffectsMetaAnalysis(
      evidenceSynthesisAnalysisId = 2,
      evidenceSynthesisSource = source
    )
  )
})

test_that("EvidenceSynthesisModule - createBayesianMetaAnalysis with default parameters", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "adaptive grid"
  )

  analysis <- esModule$createBayesianMetaAnalysis(
    evidenceSynthesisAnalysisId = 3,
    evidenceSynthesisSource = source
  )

  expect_s3_class(analysis, "BayesianMetaAnalysis")
  expect_s3_class(analysis, "EvidenceSynthesisAnalysis")
  expect_equal(analysis$chainLength, 1100000)
  expect_equal(analysis$burnIn, 1e+05)
  expect_equal(analysis$subSampleFrequency, 100)
  expect_equal(analysis$robust, FALSE)
  expect_equal(analysis$seed, 1)
})

test_that("EvidenceSynthesisModule - createBayesianMetaAnalysis with custom parameters", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "grid with gradients"
  )

  analysis <- esModule$createBayesianMetaAnalysis(
    chainLength = 500000,
    burnIn = 50000,
    subSampleFrequency = 50,
    priorSd = c(1.5, 0.3),
    alpha = 0.10,
    robust = TRUE,
    df = 5,
    seed = 42,
    evidenceSynthesisAnalysisId = 4,
    evidenceSynthesisDescription = "Bayesian Test",
    evidenceSynthesisSource = source
  )

  expect_equal(analysis$chainLength, 500000)
  expect_equal(analysis$burnIn, 50000)
  expect_equal(analysis$subSampleFrequency, 50)
  expect_equal(analysis$priorSd, c(1.5, 0.3))
  expect_equal(analysis$alpha, 0.10)
  expect_equal(analysis$robust, TRUE)
  expect_equal(analysis$df, 5)
  expect_equal(analysis$seed, 42)
})

test_that("EvidenceSynthesisModule - createEsDiagnosticThresholds with default parameters", {
  esModule <- EvidenceSynthesisModule$new()

  thresholds <- esModule$createEsDiagnosticThresholds()

  expect_s3_class(thresholds, "EsDiagnosticThresholds")
  expect_equal(thresholds$mdrrThreshold, 10)
  expect_equal(thresholds$easeThreshold, 0.25)
  expect_equal(thresholds$i2Threshold, 0.4)
  expect_equal(thresholds$tauThreshold, log(2))
  expect_null(thresholds$sdmThreshold)
  expect_null(thresholds$sdmAlpha)
})

test_that("EvidenceSynthesisModule - createEsDiagnosticThresholds with custom parameters", {
  esModule <- EvidenceSynthesisModule$new()

  thresholds <- esModule$createEsDiagnosticThresholds(
    mdrrThreshold = 5,
    easeThreshold = 0.15,
    i2Threshold = 0.5,
    tauThreshold = 1.5,
    sdmThreshold = 0.1,
    sdmAlpha = 0.05
  )

  expect_equal(thresholds$mdrrThreshold, 5)
  expect_equal(thresholds$easeThreshold, 0.15)
  expect_equal(thresholds$i2Threshold, 0.5)
  expect_equal(thresholds$tauThreshold, 1.5)
  expect_equal(thresholds$sdmThreshold, 0.1)
  expect_equal(thresholds$sdmAlpha, 0.05)
})

test_that("EvidenceSynthesisModule - createEsDiagnosticThresholds validates parameters", {
  esModule <- EvidenceSynthesisModule$new()

  # Test invalid threshold values (negative)
  expect_error(
    esModule$createEsDiagnosticThresholds(mdrrThreshold = -1)
  )

  expect_error(
    esModule$createEsDiagnosticThresholds(easeThreshold = -1)
  )
})

test_that("EvidenceSynthesisModule - createEsDiagnosticThresholds rejects sdmAlpha without sdmThreshold", {
  esModule <- EvidenceSynthesisModule$new()

  expect_error(
    esModule$createEsDiagnosticThresholds(
      sdmAlpha = 0.05
    )
  )
})

test_that("EvidenceSynthesisModule - createModuleSpecifications requires analysis list", {
  esModule <- EvidenceSynthesisModule$new()

  expect_error(
    esModule$createModuleSpecifications(
      evidenceSynthesisAnalysisList = list()
    )
  )
})

test_that("EvidenceSynthesisModule - createModuleSpecifications with valid analyses", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "normal"
  )

  analysis <- esModule$createRandomEffectsMetaAnalysis(
    evidenceSynthesisAnalysisId = 1,
    evidenceSynthesisSource = source
  )

  specs <- esModule$createModuleSpecifications(
    evidenceSynthesisAnalysisList = list(analysis)
  )

  expect_s3_class(specs, "EvidenceSynthesisModuleSpecifications")
  expect_true("settings" %in% names(specs))
  expect_true("module" %in% names(specs))
  expect_equal(specs$module, "EvidenceSynthesisModule")
})

test_that("EvidenceSynthesisModule - createModuleSpecifications with multiple analyses", {
  esModule <- EvidenceSynthesisModule$new()

  source1 <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "normal"
  )

  source2 <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "SelfControlledCaseSeries",
    likelihoodApproximation = "adaptive grid"
  )

  analysis1 <- esModule$createRandomEffectsMetaAnalysis(
    evidenceSynthesisAnalysisId = 1,
    evidenceSynthesisSource = source1
  )

  analysis2 <- esModule$createBayesianMetaAnalysis(
    evidenceSynthesisAnalysisId = 2,
    evidenceSynthesisSource = source2
  )

  specs <- esModule$createModuleSpecifications(
    evidenceSynthesisAnalysisList = list(analysis1, analysis2)
  )

  expect_s3_class(specs, "EvidenceSynthesisModuleSpecifications")
  expect_equal(length(specs$settings$evidenceSynthesisAnalysisList), 2)
})

test_that("EvidenceSynthesisModule - validateModuleSpecifications works", {
  esModule <- EvidenceSynthesisModule$new()

  source <- esModule$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "normal"
  )

  analysis <- esModule$createRandomEffectsMetaAnalysis(
    evidenceSynthesisAnalysisId = 1,
    evidenceSynthesisSource = source
  )

  specs <- esModule$createModuleSpecifications(
    evidenceSynthesisAnalysisList = list(analysis)
  )

  expect_no_error(
    esModule$validateModuleSpecifications(
      moduleSpecifications = specs
    )
  )
})

test_that("EvidenceSynthesisModule - getResultsDataModelSpecification returns data model", {
  esModule <- EvidenceSynthesisModule$new()

  dataModel <- esModule$getResultsDataModelSpecification()

  expect_s3_class(dataModel, "data.frame")
  expect_true("tableName" %in% names(dataModel))
  expect_true("columnName" %in% names(dataModel))
})

test_that("EvidenceSynthesisModule - getResultsDataModelSpecification with custom prefix", {
  esModule <- EvidenceSynthesisModule$new()

  dataModel <- esModule$getResultsDataModelSpecification(tablePrefix = "custom_")

  expect_s3_class(dataModel, "data.frame")

  # Verify that custom prefix is added
  expect_true(all(startsWith(unique(dataModel$tableName), "custom_es_")))
})

