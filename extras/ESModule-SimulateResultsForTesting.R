# This file was part of EvidenceSynthesisModule


library(dplyr)
source("extras/ESModule-SimulationFunctions.R")

databaseFile <- "inst/testdata/esmodule/results.sqlite"
if (file.exists(databaseFile)) {
  unlink(databaseFile)
}
connection <- DatabaseConnector::connect(dbms = "sqlite", server = databaseFile)

# Simulate CohortMethod data ---------------------------------------------------

targetId <- 1
comparatorId <- 2
# outcomeId <- 1
for (outcomeId in 1:26) {
  message(sprintf("Simulating outcome %d", outcomeId))
  outcomeOfInterest <- outcomeId == 1
  trueEffectSize <- if_else(outcomeOfInterest, 2, 1)
  cmTargetComparatorOutcome <- tibble(
    targetId = targetId,
    comparatorId = comparatorId,
    outcomeId = outcomeId,
    trueEffectSize = trueEffectSize,
    outcomeOfInterest = outcomeOfInterest
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "cm_target_comparator_outcome",
    data = cmTargetComparatorOutcome,
    createTable = outcomeId == 1,
    dropTableIfExists = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  for (analysisId in 1:4) {
    simulateTco(targetId, comparatorId, outcomeId, analysisId, hazardRatio = trueEffectSize)
  }
}

# Simulate SCCS data -----------------------------------------------------------
exposureId <- 100
# outcomeId <- 1
for (outcomeId in 1:26) {
  message(sprintf("Simulating outcome %d", outcomeId))
  outcomeOfInterest <- outcomeId == 1
  trueEffectSize <- if_else(outcomeOfInterest, 2, 1)

  sccsExposuresOutcomeSet <- tibble(
    exposuresOutcomeSetId = outcomeId,
    outcomeId = !!outcomeId,
    nestingCohortId = NA
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "sccs_exposures_outcome_set",
    data = sccsExposuresOutcomeSet,
    createTable = outcomeId == 1,
    dropTableIfExists = outcomeId == 1,
    camelCaseToSnakeCase = TRUE
  )

  sccsExposure <- tibble(
    exposuresOutcomeSetId = outcomeId,
    eraId = !!exposureId,
    trueEffectSize = ifelse(trueEffectSize == 1, 1, NA),
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "sccs_exposure",
    data = sccsExposure,
    createTable = outcomeId == 1,
    dropTableIfExists = outcomeId == 1,
    camelCaseToSnakeCase = TRUE
  )

  sccsCovariateAnalysis <- tibble(
    analysisId = 1:4,
    covariateAnalysisId = 1,
    variableOfInterest = 1
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "sccs_covariate_analysis",
    data = sccsCovariateAnalysis,
    createTable = outcomeId == 1,
    dropTableIfExists = outcomeId == 1,
    camelCaseToSnakeCase = TRUE
  )

  for (analysisId in 1:4) {
    simulateEo(exposureId = exposureId, outcomeId = outcomeId, analysisId = analysisId, incidenceRateRatio = trueEffectSize)
  }
}
DatabaseConnector::disconnect(connection)
