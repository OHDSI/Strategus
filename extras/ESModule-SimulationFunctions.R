# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of EvidenceSynthesisModule
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Functions used in SimulateResultsForTestin.R

library(survival)

simulateTco <- function(targetId, comparatorId, outcomeId, analysisId, hazardRatio = 1, nSites = 10) {
  simulationSettings <- EvidenceSynthesis::createSimulationSettings(
    nSites = nSites,
    n = 2500,
    treatedFraction = 0.25,
    hazardRatio = hazardRatio,
    randomEffectSd = if_else(hazardRatio == 1, 0, 0.5)
  )
  cmDiagnosticsSummary <- tibble(
    targetId = targetId,
    comparatorId = comparatorId,
    outcomeId = outcomeId,
    analysisId = analysisId,
    databaseId = seq_len(nSites),
    mdrr = 2,
    unblind = runif(nSites) < 0.9
  )

  populations <- EvidenceSynthesis::simulatePopulations(simulationSettings)
  cmResult <- list()
  cmLikelihoodProfile <- list()
  # i = 1
  for (i in seq_along(populations)) {
    population <- populations[[i]]
    cyclopsData <- Cyclops::createCyclopsData(Surv(time, y) ~ x + strata(stratumId),
      data = population,
      modelType = "cox"
    )
    cyclopsFit <- Cyclops::fitCyclopsModel(cyclopsData)
    ci <- tryCatch(
      {
        confint(cyclopsFit, parm = 1, includePenalty = TRUE)
      },
      error = function(e) {
        c(0, -Inf, Inf)
      }
    )
    normal <- EvidenceSynthesis::approximateLikelihood(cyclopsFit, "x", approximation = "normal")
    adaptiveGrid <- EvidenceSynthesis::approximateLikelihood(cyclopsFit, "x", approximation = "adaptive grid")
    z <- normal$logRr / normal$seLogRr
    p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
    cmResult[[i]] <- tibble(
      targetId = targetId,
      comparatorId = comparatorId,
      outcomeId = outcomeId,
      analysisId = analysisId,
      databaseId = i,
      targetSubjects = sum(population$x == 1),
      comparatorSubjects = sum(population$x == 0),
      targetDays = sum(population$time[population$x == 1]),
      comparatorDays = sum(population$time[population$x == 0]),
      targetOutcomes = sum(population$y[population$x == 1]),
      comparatorOutcomes = sum(population$y[population$x == 0]),
      rr = exp(normal$logRr),
      ci95Lb = exp(ci[2]),
      ci95Ub = exp(ci[3]),
      p = p,
      logRr = normal$logRr,
      seLogRr = normal$seLogRr
    )
    cmLikelihoodProfile[[i]] <- adaptiveGrid %>%
      rename(
        logRr = .data$point,
        logLikelihood = .data$value
      ) %>%
      mutate(
        targetId = targetId,
        comparatorId = comparatorId,
        outcomeId = outcomeId,
        analysisId = analysisId,
        databaseId = i
      )
  }
  cmResult <- bind_rows(cmResult)
  cmLikelihoodProfile <- bind_rows(cmLikelihoodProfile)
  tablesExist <- DatabaseConnector::existsTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "cm_diagnostics_summary"
  )

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "cm_diagnostics_summary",
    data = cmDiagnosticsSummary,
    createTable = !tablesExist,
    dropTableIfExists = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "cm_result",
    data = cmResult,
    createTable = !tablesExist,
    dropTableIfExists = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "cm_likelihood_profile",
    data = cmLikelihoodProfile,
    createTable = !tablesExist,
    dropTableIfExists = FALSE,
    camelCaseToSnakeCase = TRUE
  )
}

simulateEo <- function(exposureId, outcomeId, analysisId, incidenceRateRatio = 1, nSites = 10) {
  simulationRiskWindows <- list(SelfControlledCaseSeries::createSimulationRiskWindow(relativeRisks = incidenceRateRatio))

  simulationSettings <- SelfControlledCaseSeries::createSccsSimulationSettings(
    eraIds = exposureId,
    simulationRiskWindows = simulationRiskWindows,
    outcomeId = outcomeId
  )
  sccsResult <- list()
  sccsLikelihoodProfile <- list()
  for (i in seq_len(nSites)) {
    sccsData <- SelfControlledCaseSeries::simulateSccsData(
      nCases = 1000,
      settings = simulationSettings
    )
    studyPop <- SelfControlledCaseSeries::createStudyPopulation(
      sccsData = sccsData,
      outcomeId = outcomeId,
      firstOutcomeOnly = TRUE,
      naivePeriod = 180
    )
    eraCovariateSettings <- SelfControlledCaseSeries::createEraCovariateSettings(
      includeEraIds = exposureId,
      profileLikelihood = TRUE
    )
    sccsIntervalData <- SelfControlledCaseSeries::createSccsIntervalData(
      studyPopulation = studyPop,
      sccsData = sccsData,
      eraCovariateSettings = eraCovariateSettings
    )
    model <- SelfControlledCaseSeries::fitSccsModel(
      sccsIntervalData = sccsIntervalData
    )
    covariateSettings <- model$metaData$covariateSettingsList[[1]]
    attrition <- model$metaData$attrition[nrow(model$metaData$attrition), ]
    covariateStatistics <- model$metaData$covariateStatistics
    estimate <- model$estimates
    if (is.null(estimate) || nrow(estimate) == 0) {
      p <- NA
    } else {
      z <- estimate$logRr / estimate$seLogRr
      p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
    }
    sccsResult[[i]] <- tibble(
      exposuresOutcomeSetId = outcomeId,
      analysisId = analysisId,
      covariateId = covariateSettings$outputIds[1],
      outcomeSubjects = attrition$outcomeSubjects,
      outcomeEvents = attrition$outcomeEvents,
      outcomeObservationPeriods = attrition$outcomeObsPeriods,
      observedDays = attrition$observedDays,
      covariateSubjects = ifelse(nrow(covariateStatistics) == 0, 0, covariateStatistics$personCount),
      covariateDays = ifelse(nrow(covariateStatistics) == 0, 0, covariateStatistics$dayCount),
      covariateEras = ifelse(nrow(covariateStatistics) == 0, 0, covariateStatistics$eraCount),
      covariateOutcomes = ifelse(nrow(covariateStatistics) == 0, 0, covariateStatistics$outcomeCount),
      rr = ifelse(nrow(estimate) == 0, NA, exp(estimate$logRr)),
      ci95Lb = ifelse(nrow(estimate) == 0, NA, exp(estimate$logLb95)),
      ci95Ub = ifelse(nrow(estimate) == 0, NA, exp(estimate$logUb95)),
      p = p,
      logRr = ifelse(nrow(estimate) == 0, NA, estimate$logRr),
      seLogRr = ifelse(nrow(estimate) == 0, NA, estimate$seLogRr),
      llr = ifelse(nrow(estimate) == 0, NA, estimate$llr),
      databaseId = i
    )

    sccsLikelihoodProfile[[i]] <- model$logLikelihoodProfiles[[1]] %>%
      rename(
        logRr = "point",
        logLikelihood = "value"
      ) %>%
      mutate(
        exposuresOutcomeSetId = outcomeId,
        covariateId = covariateSettings$outputIds[1],
        outcomeId = outcomeId,
        analysisId = analysisId,
        databaseId = i
      )
  }
  sccsResult <- bind_rows(sccsResult)
  sccsLikelihoodProfile <- bind_rows(sccsLikelihoodProfile)
  tablesExist <- DatabaseConnector::existsTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "sccs_diagnostics_summary"
  )

  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "sccs_result",
    data = sccsResult,
    createTable = !tablesExist,
    dropTableIfExists = FALSE,
    camelCaseToSnakeCase = TRUE
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "sccs_likelihood_profile",
    data = sccsLikelihoodProfile,
    createTable = !tablesExist,
    dropTableIfExists = FALSE,
    camelCaseToSnakeCase = TRUE
  )

  sccsDiagnosticsSummary <- tibble(
    covariateId = 1000,
    exposuresOutcomeSetId = !!outcomeId,
    analysisId = !!analysisId,
    databaseId = seq_len(nSites),
    mdrr = 2,
    unblind = runif(nSites) < 0.9
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "sccs_diagnostics_summary",
    data = sccsDiagnosticsSummary,
    createTable = !tablesExist,
    dropTableIfExists = FALSE,
    camelCaseToSnakeCase = TRUE
  )

  sccsCovariate <- tibble(
    eraId = !!exposureId,
    exposuresOutcomeSetId = !!outcomeId,
    analysisId = !!analysisId,
    databaseId = seq_len(nSites),
    covariateId = 1000,
    covariateAnalysisId = 1
  )
  DatabaseConnector::insertTable(
    connection = connection,
    databaseSchema = "main",
    tableName = "sccs_covariate",
    data = sccsCovariate,
    createTable = !tablesExist,
    dropTableIfExists = FALSE,
    camelCaseToSnakeCase = TRUE
  )
}
