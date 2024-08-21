library(testthat)
library(Strategus)

esTestDataConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = system.file("testdata/esmodule/results.sqlite", package = "Strategus")
)

workFolder <- tempfile("work")
dir.create(workFolder)
testResultsFolder <- tempfile("results")
dir.create(testResultsFolder)
# jobContext <- readRDS("tests/testJobContext.rds")
# jobContext$moduleExecutionSettings$workSubFolder <- workFolder
# jobContext$moduleExecutionSettings$resultsSubFolder <- testResultsFolder
# jobContext$moduleExecutionSettings$resultsConnectionDetails <- esTestDataConnectionDetails

createEsModuleSpecs <- function() {
  esModuleSettingsCreator <- EvidenceSynthesisModule$new()
  # Create EvidenceSynthesisModule settings ---------------------------------------

  evidenceSynthesisSourceCmGrid <-  esModuleSettingsCreator$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    likelihoodApproximation = "adaptive grid"
  )

  evidenceSynthesisSourceCmNormal <- esModuleSettingsCreator$createEvidenceSynthesisSource(
    sourceMethod = "CohortMethod",
    databaseIds = c(1, 2, 4),
    analysisIds = c(1, 3),
    likelihoodApproximation = "normal"
  )

  evidenceSynthesisSourceSccsGrid <- esModuleSettingsCreator$createEvidenceSynthesisSource(
    sourceMethod = "SelfControlledCaseSeries",
    likelihoodApproximation = "adaptive grid"
  )

  evidenceSynthesisSourceSccsNormal <- esModuleSettingsCreator$createEvidenceSynthesisSource(
    sourceMethod = "SelfControlledCaseSeries",
    databaseIds = c(1, 2, 4),
    analysisIds = c(1, 3),
    likelihoodApproximation = "normal"
  )

  fixedEffectsMetaAnalysisCm <- esModuleSettingsCreator$createFixedEffectsMetaAnalysis(
    evidenceSynthesisAnalysisId = 1,
    evidenceSynthesisSource = evidenceSynthesisSourceCmNormal
  )

  randomEffectsMetaAnalysisCm <- esModuleSettingsCreator$createRandomEffectsMetaAnalysis(
    evidenceSynthesisAnalysisId = 2,
    evidenceSynthesisSource = evidenceSynthesisSourceCmNormal
  )

  bayesianMetaAnalysisCm <- esModuleSettingsCreator$createBayesianMetaAnalysis(
    evidenceSynthesisAnalysisId = 3,
    evidenceSynthesisSource = evidenceSynthesisSourceCmGrid
  )

  fixedEffectsMetaAnalysisSccs <- esModuleSettingsCreator$createFixedEffectsMetaAnalysis(
    evidenceSynthesisAnalysisId = 4,
    evidenceSynthesisSource = evidenceSynthesisSourceSccsNormal
  )

  randomEffectsMetaAnalysisSccs <- esModuleSettingsCreator$createRandomEffectsMetaAnalysis(
    evidenceSynthesisAnalysisId = 5,
    evidenceSynthesisSource = evidenceSynthesisSourceSccsNormal
  )

  bayesianMetaAnalysisSccs <- esModuleSettingsCreator$createBayesianMetaAnalysis(
    evidenceSynthesisAnalysisId = 6,
    evidenceSynthesisSource = evidenceSynthesisSourceSccsGrid
  )

  evidenceSynthesisModuleSpecs <- esModuleSettingsCreator$createModuleSpecifications(
    evidenceSynthesisAnalysisList = list(
      fixedEffectsMetaAnalysisCm,
      randomEffectsMetaAnalysisCm,
      bayesianMetaAnalysisCm,
      fixedEffectsMetaAnalysisSccs,
      randomEffectsMetaAnalysisSccs,
      bayesianMetaAnalysisSccs
    )
  )

  analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
    addModuleSpecifications(evidenceSynthesisModuleSpecs)

  return(analysisSpecifications)
}

test_that("Run module", {
  esAnalysisSpecifications <- createEsModuleSpecs()
  resultsExecutionSettings <- Strategus::createResultsExecutionSettings(
    resultsDatabaseSchema = "main",
    resultsFolder = testResultsFolder,
    workFolder = workFolder
  )
  Strategus::execute(
    analysisSpecifications = esAnalysisSpecifications,
    executionSettings = resultsExecutionSettings,
    connectionDetails = esTestDataConnectionDetails
  )
  resultsFiles <- list.files(file.path(testResultsFolder, "EvidenceSynthesisModule"))
  expect_true("es_analysis.csv" %in% resultsFiles)
  expect_true("es_cm_result.csv" %in% resultsFiles)
  expect_true("es_cm_diagnostics_summary.csv" %in% resultsFiles)
  expect_true("es_sccs_result.csv" %in% resultsFiles)
  expect_true("es_sccs_diagnostics_summary.csv" %in% resultsFiles)
})

test_that("Skipped analyses as specified", {
  # We specified we didn't want cohort method analysis ID 2 in evidence synthesis ID 2:
  results <- CohortGenerator::readCsv(file.path(testResultsFolder, "es_cm_result.csv"))
  expect_false(any(results$evidenceSynthesisAnalysisId == 2 & results$analysisId == 2))

  results <- CohortGenerator::readCsv(file.path(testResultsFolder, "es_sccs_result.csv"))
  expect_false(any(results$evidenceSynthesisAnalysisId == 2 & results$analysisId == 2))
})

getApproximation <- function(setting) {
  tibble(
    evidenceSynthesisAnalysisId = setting$evidenceSynthesisAnalysisId,
    likelihoodApproximation = setting$evidenceSynthesisSource$likelihoodApproximation
  ) %>%
    return()
}

getDatabaseIds <- function(setting, databaseIds) {
  if (!is.null(setting$evidenceSynthesisSource$databaseIds)) {
    databaseIds <- setting$evidenceSynthesisSource$databaseIds
  }
  tibble(
    evidenceSynthesisAnalysisId = setting$evidenceSynthesisAnalysisId,
    databaseId = databaseIds
  ) %>%
    return()
}

test_that("Include only allowed CM estimates in meta-analysis", {
  # Should only include estimates in meta-analysis that are
  # 1. Either unblinded or not outcome of interest
  # 2. Has a valid estimate (normal approx) or LL profile (adaptive grid)
  # 3. Is not excluded in createEvidenceSynthesisSource()
  connection <- DatabaseConnector::connect(esTestDataConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Determine if unblinded:
  sql <- "
    SELECT cm_target_comparator_outcome.target_id,
      cm_target_comparator_outcome.comparator_id,
      cm_target_comparator_outcome.outcome_id,
      analysis_id,
      database_id,
      unblind AS include_1
    FROM main.cm_target_comparator_outcome
    LEFT JOIN main.cm_diagnostics_summary
      ON cm_diagnostics_summary.target_id = cm_target_comparator_outcome.target_id
        AND cm_diagnostics_summary.comparator_id = cm_target_comparator_outcome.comparator_id
        AND cm_diagnostics_summary.outcome_id = cm_target_comparator_outcome.outcome_id;
  "
  criterion1 <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Must have some blinded results for this test to work:
  expect_gt(sum(criterion1$include1 == 0), 0)

  # Determine if valid estimate or LL profile:
  approximations <- bind_rows(lapply(jobContext$settings$evidenceSynthesisAnalysisList, getApproximation))
  sql <- "
    SELECT cm_result.target_id,
      cm_result.comparator_id,
      cm_result.outcome_id,
      cm_result.analysis_id,
      cm_result.database_id,
      CASE
        WHEN log_rr IS NOT NULL AND se_log_rr IS NOT NULL THEN 1
        ELSE 0
      END AS has_valid_estimate,
      CASE
        WHEN profiles.target_id IS NOT NULL THEN 1
        ELSE 0
      END AS has_ll_profile
    FROM main.cm_result
    LEFT JOIN (
      SELECT DISTINCT target_id,
        comparator_id,
        outcome_id,
        analysis_id,
        database_id
      FROM main.cm_likelihood_profile
      ) profiles
      ON cm_result.target_id = profiles.target_id
        AND cm_result.comparator_id = profiles.comparator_id
        AND cm_result.outcome_id = profiles.outcome_id
        AND cm_result.analysis_id = profiles.analysis_id
        AND cm_result.database_id = profiles.database_id
  "
  criterion2 <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE) %>%
    cross_join(approximations) %>%
    mutate(include2 = if_else(likelihoodApproximation == "normal",
      hasValidEstimate,
      hasLlProfile
    ))

  # Determine if database was excluded in createEvidenceSynthesisSource():
  databaseIds <- unique(criterion2$databaseId)
  criterion3 <- bind_rows(lapply(jobContext$settings$evidenceSynthesisAnalysisList, getDatabaseIds, databaseIds = databaseIds)) %>%
    mutate(include3 = 1)

  # Combine all criteria, and check if agree with results:
  allowed <- criterion1 %>%
    inner_join(criterion2,
      by = join_by(targetId, comparatorId, outcomeId, analysisId, databaseId),
      relationship = "one-to-many"
    ) %>%
    inner_join(criterion3, by = join_by(databaseId, evidenceSynthesisAnalysisId)) %>%
    mutate(include = include1 & include2 & include3) %>%
    group_by(targetId, comparatorId, outcomeId, analysisId, evidenceSynthesisAnalysisId) %>%
    summarize(nAllowed = sum(include), .groups = "drop")

  results <- CohortGenerator::readCsv(file.path(testResultsFolder, "EvidenceSynthesisModule", "es_cm_result.csv"))
  results <- results %>%
    left_join(allowed, by = join_by(targetId, comparatorId, outcomeId, analysisId, evidenceSynthesisAnalysisId))
  expect_true(all(results$nDatabases == results$nAllowed))
})

test_that("Include only allowed SCCS estimates in meta-analysis", {
  # Should only include estimates in meta-analysis that are
  # 1. Unblinded.
  # 2. Has a valid estimate (normal approx) or LL profile (adaptive grid)
  # 3. Is not excluded in createEvidenceSynthesisSource()
  connection <- DatabaseConnector::connect(esTestDataConnectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))

  # Determine if unblinded or true effect size is known:
  sql <- "
    SELECT sccs_diagnostics_summary.exposures_outcome_set_id,
      sccs_diagnostics_summary.covariate_id,
      sccs_diagnostics_summary.analysis_id,
      sccs_diagnostics_summary.database_id,
      unblind AS include_1
    FROM main.sccs_exposure
    INNER JOIN main.sccs_diagnostics_summary
      ON sccs_exposure.exposures_outcome_set_id = sccs_diagnostics_summary.exposures_outcome_set_id
    INNER JOIN main.sccs_covariate
      ON sccs_exposure.era_id = sccs_covariate.era_id
        AND sccs_covariate.covariate_id = sccs_diagnostics_summary.covariate_id
        AND sccs_covariate.exposures_outcome_set_id = sccs_diagnostics_summary.exposures_outcome_set_id
        AND sccs_covariate.analysis_id = sccs_diagnostics_summary.analysis_id
        AND sccs_covariate.database_id = sccs_diagnostics_summary.database_id;
  "
  criterion1 <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE)
  # Must have some blinded results for this test to work:
  expect_gt(sum(criterion1$include1 == 0), 0)

  # Determine if valid estimate or LL profile:
  approximations <- bind_rows(lapply(jobContext$settings$evidenceSynthesisAnalysisList, getApproximation))
  sql <- "
    SELECT sccs_result.exposures_outcome_set_id,
      sccs_result.covariate_id,
      sccs_result.analysis_id,
      sccs_result.database_id,
      CASE
        WHEN log_rr IS NOT NULL AND se_log_rr IS NOT NULL THEN 1
        ELSE 0
      END AS has_valid_estimate,
      CASE
        WHEN profiles.exposures_outcome_set_id IS NOT NULL THEN 1
        ELSE 0
      END AS has_ll_profile
    FROM main.sccs_result
    LEFT JOIN (
      SELECT DISTINCT exposures_outcome_set_id,
        covariate_id,
        analysis_id,
        database_id
      FROM main.sccs_likelihood_profile
      ) profiles
      ON sccs_result.exposures_outcome_set_id = profiles.exposures_outcome_set_id
        AND sccs_result.covariate_id = profiles.covariate_id
        AND sccs_result.analysis_id = profiles.analysis_id
        AND sccs_result.database_id = profiles.database_id
  "
  criterion2 <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE) %>%
    cross_join(approximations) %>%
    mutate(include2 = if_else(likelihoodApproximation == "normal",
      hasValidEstimate,
      hasLlProfile
    ))

  # Determine if database was excluded in createEvidenceSynthesisSource():
  databaseIds <- unique(criterion2$databaseId)
  criterion3 <- bind_rows(lapply(jobContext$settings$evidenceSynthesisAnalysisList, getDatabaseIds, databaseIds = databaseIds)) %>%
    mutate(include3 = 1)

  # Combine all criteria, and check if agree with results:
  allowed <- criterion1 %>%
    inner_join(criterion2,
      by = join_by(exposuresOutcomeSetId, covariateId, analysisId, databaseId),
      relationship = "one-to-many"
    ) %>%
    inner_join(criterion3, by = join_by(databaseId, evidenceSynthesisAnalysisId)) %>%
    mutate(include = include1 & include2 & include3) %>%
    group_by(exposuresOutcomeSetId, covariateId, analysisId, evidenceSynthesisAnalysisId) %>%
    summarize(nAllowed = sum(include), .groups = "drop")

  results <- CohortGenerator::readCsv(file.path(testResultsFolder, "EvidenceSynthesisModule", "es_sccs_result.csv"))
  results <- results %>%
    left_join(allowed, by = join_by(exposuresOutcomeSetId, covariateId, analysisId, evidenceSynthesisAnalysisId))
  expect_true(all(results$nDatabases == results$nAllowed))
})

test_that("Output conforms to results model", {
  model <- CohortGenerator::readCsv(file.path(testResultsFolder, "EvidenceSynthesisModule", "resultsDataModelSpecification.csv"))
  tables <- unique(model$tableName)
  for (table in tables) {
    data <- readr::read_csv(file.path(testResultsFolder, sprintf("%s.csv", table)), show_col_types = FALSE)
    observed <- colnames(data)
    observed <- sort(observed)
    expected <- model$columnName[model$tableName == table]
    expected <- sort(expected)
    expect_equal(observed, expected)
  }
})

test_that("Check MDRR values", {
  # CohortMethod
  results <- CohortGenerator::readCsv(file.path(testResultsFolder, "EvidenceSynthesisModule", "es_cm_result.csv"))
  diagnostics <- CohortGenerator::readCsv(file.path(testResultsFolder, "EvidenceSynthesisModule", "es_cm_diagnostics_summary.csv"))
  combined <- results %>%
    inner_join(diagnostics, by = join_by(targetId, comparatorId, outcomeId, analysisId, evidenceSynthesisAnalysisId))
  noDbs <- combined %>%
    filter(nDatabases == 0)
  expect_true(all(is.infinite(noDbs$mdrr)))
  expect_true(all(noDbs$mdrrDiagnostic == "FAIL"))
  expect_true(all(noDbs$unblind == 0))

  oneDb <- combined %>%
    filter(nDatabases == 1)
  # All per-DB MDRRs were set to 2 in simulation code:
  expect_true(all(oneDb$mdrr == 2))
  expect_true(all(oneDb$mdrrDiagnostic == "PASS"))

  multiDbs <- combined %>%
    filter(nDatabases > 1, !is.na(seLogRr))

  expect_true(all(!is.na(multiDbs$mdrr)))

  # SCCS
  results <- CohortGenerator::readCsv(file.path(testResultsFolder, "EvidenceSynthesisModule", "es_sccs_result.csv"))
  diagnostics <- CohortGenerator::readCsv(file.path(testResultsFolder, "EvidenceSynthesisModule", "es_sccs_diagnostics_summary.csv"))
  combined <- results %>%
    inner_join(diagnostics, by = join_by(analysisId, exposuresOutcomeSetId, covariateId, evidenceSynthesisAnalysisId))
  noDbs <- combined %>%
    filter(nDatabases == 0)
  expect_true(all(is.infinite(noDbs$mdrr)))
  expect_true(all(noDbs$mdrrDiagnostic == "FAIL"))
  expect_true(all(noDbs$unblind == 0))

  oneDb <- combined %>%
    filter(nDatabases == 1)
  # All per-DB MDRRs were set to 2 in simulation code:
  expect_true(all(oneDb$mdrr == 2))
  expect_true(all(oneDb$mdrrDiagnostic == "PASS"))

  multiDbs <- combined %>%
    filter(nDatabases > 1, !is.na(seLogRr))

  expect_true(all(!is.na(multiDbs$mdrr)))
})

test_that("Don't error when no negative controls present", {
  # Create dataset without negative controls
  tempFile <- tempfile(fileext = ".sqlite")
  file.copy(system.file("testdata/esmodule/results.sqlite", package = "Strategus"), tempFile)
  on.exit(unlink(tempFile))
  tempConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = tempFile
  )
  connection <- DatabaseConnector::connect(tempConnectionDetails)
  DatabaseConnector::renderTranslateExecuteSql(connection, "UPDATE cm_target_comparator_outcome SET true_effect_size = NULL;")
  DatabaseConnector::disconnect(connection)

  # tempJobContext <- jobContext
  # tempJobContext$settings$evidenceSynthesisAnalysisList <- list(tempJobContext$settings$evidenceSynthesisAnalysisList[[1]])
  # tempJobContext$moduleExecutionSettings$resultsConnectionDetails <- tempConnectionDetails
  # execute(tempJobContext)

  esAnalysisSpecifications <- createEsModuleSpecs()
  esAnalysisSpecifications$moduleSpecifications[[1]]$settings$evidenceSynthesisAnalysisList <- list(esAnalysisSpecifications$moduleSpecifications[[1]]$settings$evidenceSynthesisAnalysisList[[1]])
  resultsExecutionSettings <- Strategus::createResultsExecutionSettings(
    resultsDatabaseSchema = "main",
    resultsFolder = testResultsFolder,
    workFolder = workFolder
  )
  Strategus::execute(
    analysisSpecifications = esAnalysisSpecifications,
    executionSettings = resultsExecutionSettings,
    connectionDetails = tempConnectionDetails
  )

  estimates <- readr::read_csv(file.path(testResultsFolder, "EvidenceSynthesisModule", "es_cm_result.csv"), show_col_types = FALSE)
  expect_gt(nrow(estimates), 0)
  expect_true(all(is.na(estimates$calibrated_rr)))
})


# readr::write_csv(OhdsiRTools::createResultsSchemaStub(testResultsFolder), "resultsDataModelSpecification.csv")

unlink(workFolder)
unlink(testResultsFolder)
