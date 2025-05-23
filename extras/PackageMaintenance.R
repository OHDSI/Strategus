# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of Strategus
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

# Go document the full results data model -> DocumentResultsDataModel.R

# Manually delete package from library. Avoids "Already in use" message when rebuilding
unloadNamespace("Strategus")
.rs.restartR()
folder <- system.file(package = "Strategus")
folder
unlink(folder, recursive = TRUE, force = TRUE)
file.exists(folder)

# Format and check code:
styler::style_pkg()
OhdsiRTools::checkUsagePackage("Strategus")
OhdsiRTools::updateCopyrightYearFolder()
OhdsiRTools::findNonAsciiStringsInFolder()
devtools::spell_check()
devtools::document()

# Create manual and vignettes:
unlink("extras/Strategus.pdf")
shell("R CMD Rd2pdf ./ --output=extras/Strategus.pdf")

dir.create("inst/doc")
rmarkdown::render("vignettes/CreatingAnalysisSpecification.Rmd",
                  output_file = "../inst/doc/CreatingAnalysisSpecification.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/CreatingAnalysisSpecification.tex")

rmarkdown::render("vignettes/ExecuteStrategus.Rmd",
                  output_file = "../inst/doc/ExecuteStrategus.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/ExecuteStrategus.tex")

rmarkdown::render("vignettes/IntroductionToStrategus.Rmd",
                  output_file = "../inst/doc/IntroductionToStrategus.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/IntroductionToStrategus.tex")

rmarkdown::render("vignettes/WorkingWithResults.Rmd",
                  output_file = "../inst/doc/WorkingWithResults.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/WorkingWithResults.tex")

# Run pkgdown to verify there are no site build errors ---------
pkgdown::build_site()

# Produce a study analysis specification for testing -----------
library(Strategus)
source("tests/testthat/helper-TreatmentPatterns.R") # Needed for creating settings
cohortDefinitionSet <- getCohortDefinitionSet(
  settingsFileName = system.file("testdata/Cohorts.csv", package = "Strategus"),
  jsonFolder = system.file("testdata/cohorts", package = "Strategus"),
  sqlFolder = system.file("testdata/sql", package = "Strategus")
)

# Add TreatmentPatterns cohorts to the cohort definition set
cohortDefinitionSet <- cohortDefinitionSet[, names(CohortGenerator::createEmptyCohortDefinitionSet())]
cohortDefinitionSet <- appendTreatmentPatternsCohorts(cohortDefinitionSet)

subsetOperations <- list(
  createDemographicSubset(
    name = "Age 18 to 64",
    ageMin = 18,
    ageMax = 64
  )
)
subsetDef <- createCohortSubsetDefinition(
  name = "age 18 to 64",
  definitionId = 1,
  subsetOperators = subsetOperations
)
cohortDefinitionSet <- cohortDefinitionSet |>
  addCohortSubsetDefinition(subsetDef,
                            targetCohortIds = c(1,2))

subsetOperations <- list(CohortGenerator::createLimitSubset(
  name = 'first event with 365 prior obs',
  priorTime = 365,
  limitTo = 'firstEver'
  )
)
subsetDef <- createCohortSubsetDefinition(
  name = 'first event with 365 prior obs',
  definitionId = 2,
  subsetOperators = subsetOperations
)
cohortDefinitionSet <- cohortDefinitionSet |>
  addCohortSubsetDefinition(
    subsetDef,
    targetCohortIds = c(1,2)
      )

subsetOperations <- list(
  createDemographicSubset(
    name = "Age 18 to 64",
    ageMin = 18,
    ageMax = 64
  ),
  CohortGenerator::createLimitSubset(
  name = 'first event with 365 prior obs',
  priorTime = 365,
  limitTo = 'firstEver'
)
)
subsetDef <- createCohortSubsetDefinition(
  name = 'age 18 to 64 and first event with 365 prior obs',
  definitionId = 3,
  subsetOperators = subsetOperations
)
cohortDefinitionSet <- cohortDefinitionSet |>
  addCohortSubsetDefinition(
    subsetDef,
    targetCohortIds = c(1,2)
  )

# expand negative controls to get calibrated sccs (prior had too few results)
ncoCohortSet <- readCsv(file = system.file("testdata/negative_controls_concept_set.csv",
                                           package = "Strategus"
))

# Exposures-outcomes
negativeControlOutcomeIds <- ncoCohortSet$cohortId
outcomeOfInterestIds <- c(3)
exposureOfInterestIds <- c(1, 2)

# Cohort Diagnostics -----------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cdModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runTemporalCohortCharacterization = TRUE
)

# Cohort Generator -----------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create the settings & validate them
cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

ncoCohortSharedResourceSpecifications <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSet, "first", TRUE)
cgModuleSettingsCreator$validateNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSharedResourceSpecifications)

cgModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications()

# Characterization -------------------------------
cModuleSettingsCreator <- CharacterizationModule$new()
cModuleSpecifications <- cModuleSettingsCreator$createModuleSpecifications(
  targetIds = c(1, 2, 1001, 2001),
  outcomeIds = 3
)

# Cohort Incidence -----------------
ciModuleSettingsCreator <- CohortIncidenceModule$new()
targets <- list(
  CohortIncidence::createCohortRef(id = 1, name = "Celecoxib"),
  CohortIncidence::createCohortRef(id = 2, name = "Diclofenac"),
  CohortIncidence::createCohortRef(id = 1001, name = "Celecoxib Age 18-64"),
  CohortIncidence::createCohortRef(id = 2001, name = "Diclofenac Age 18-64")
)
outcomes <- list(CohortIncidence::createOutcomeDef(id = 1, name = "GI bleed", cohortId = 3, cleanWindow = 9999))

tars <- list(
  CohortIncidence::createTimeAtRiskDef(id = 1, startWith = "start", endWith = "end"),
  CohortIncidence::createTimeAtRiskDef(id = 2, startWith = "start", endWith = "start", endOffset = 365)
)
analysis1 <- CohortIncidence::createIncidenceAnalysis(
  targets = c(1, 2, 1001, 2001),
  outcomes = c(1),
  tars = c(1, 2)
)

irDesign <- CohortIncidence::createIncidenceDesign(
  targetDefs = targets,
  outcomeDefs = outcomes,
  tars = tars,
  analysisList = list(analysis1),
  strataSettings = CohortIncidence::createStrataSettings(
    byYear = TRUE,
    byGender = TRUE
  )
)

ciModuleSpecifications <- ciModuleSettingsCreator$createModuleSpecifications(
  irDesign = irDesign$toList()
)

# Treatment Patterns --------------------
treatmentPatternsCohorts <- getTreatmentPatternsCohorts(cohortDefinitionSet)
tpModuleSettingsCreator <- TreatmentPatternsModule$new()
tpModuleSpecifications <- tpModuleSettingsCreator$createModuleSpecifications(
  cohorts = treatmentPatternsCohorts,
  includeTreatments = "startDate",
  indexDateOffset = 0,
  minEraDuration = 7,
  splitEventCohorts = NULL,
  splitTime = NULL,
  eraCollapseSize = 14,
  combinationWindow = 7,
  minPostCombinationDuration = 7,
  filterTreatments = "First",
  maxPathLength = 5
)

# Cohort Method ----------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
negativeControlOutcomes <- lapply(
  X = ncoCohortSet$cohortId,
  FUN = CohortMethod::createOutcome,
  outcomeOfInterest = FALSE,
  trueEffectSize = 1,
  priorOutcomeLookback = 5 # reduced this as some neg controls are indications
)

outcomesOfInterest <- lapply(
  X = 3,
  FUN = CohortMethod::createOutcome,
  outcomeOfInterest = TRUE
)

outcomes <- append(
  negativeControlOutcomes,
  outcomesOfInterest
)

tcos1 <- CohortMethod::createTargetComparatorOutcomes(
  targetId = 1002,
  comparatorId = 2002,
  outcomes = outcomes,
  excludedCovariateConceptIds = c(1118084, 1124300)
)
tcos2 <- CohortMethod::createTargetComparatorOutcomes(
  targetId = 1003,
  comparatorId = 2003,
  outcomes = outcomes,
  excludedCovariateConceptIds = c(1118084, 1124300)
)

targetComparatorOutcomesList <- list(tcos1, tcos2)

covarSettings <- FeatureExtraction::createDefaultCovariateSettings(addDescendantsToExclude = TRUE)

getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
  washoutPeriod = 183,
  firstExposureOnly = TRUE,
  removeDuplicateSubjects = "remove all",
  maxCohortSize = 100000,
  covariateSettings = covarSettings
)

createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
  minDaysAtRisk = 1,
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 365, # expanding to get more outcomes
  endAnchor = "cohort end"
)

matchOnPsArgs <- CohortMethod::createMatchOnPsArgs()
fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(modelType = "cox")
createPsArgs <- CohortMethod::createCreatePsArgs(
  stopOnError = FALSE,
  control = Cyclops::createControl(cvRepetitions = 1)
)
computeSharedCovBalArgs <- CohortMethod::createComputeCovariateBalanceArgs()
computeCovBalArgs <- CohortMethod::createComputeCovariateBalanceArgs(
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)

cmAnalysis1 <- CohortMethod::createCmAnalysis(
  analysisId = 1,
  description = "No matching, simple outcome model",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs
)

cmAnalysis2 <- CohortMethod::createCmAnalysis(
  analysisId = 2,
  description = "Matching on ps and covariates, simple outcomeModel",
  getDbCohortMethodDataArgs = getDbCmDataArgs,
  createStudyPopArgs = createStudyPopArgs,
  createPsArgs = createPsArgs,
  matchOnPsArgs = matchOnPsArgs,
  computeSharedCovariateBalanceArgs = computeSharedCovBalArgs,
  computeCovariateBalanceArgs = computeCovBalArgs,
  fitOutcomeModelArgs = fitOutcomeModelArgs
)

cmAnalysisList <- list(cmAnalysis1, cmAnalysis2)

analysesToExclude <- NULL

cmModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = analysesToExclude,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds(
    mdrrThreshold = Inf,
    easeThreshold = 0.60, # setting this higher to get passes given Eunomia limitations on neg controls
    sdmThreshold = 0.1,
    equipoiseThreshold = 0.2,
    generalizabilitySdmThreshold = 1 # NOTE using default here
  )

)

# EvidenceSythesis ------------------
esModuleSettingsCreator = EvidenceSynthesisModule$new()
evidenceSynthesisSourceCm <- esModuleSettingsCreator$createEvidenceSynthesisSource(
  sourceMethod = "CohortMethod",
  likelihoodApproximation = "adaptive grid"
)
metaAnalysisCm <- esModuleSettingsCreator$createBayesianMetaAnalysis(
  evidenceSynthesisAnalysisId = 1,
  alpha = 0.05,
  evidenceSynthesisDescription = "Bayesian random-effects alpha 0.05 - adaptive grid",
  evidenceSynthesisSource = evidenceSynthesisSourceCm
)
evidenceSynthesisSourceSccs <- esModuleSettingsCreator$createEvidenceSynthesisSource(
  sourceMethod = "SelfControlledCaseSeries",
  likelihoodApproximation = "adaptive grid"
)
metaAnalysisSccs <- esModuleSettingsCreator$createBayesianMetaAnalysis(
  evidenceSynthesisAnalysisId = 2,
  alpha = 0.05,
  evidenceSynthesisDescription = "Bayesian random-effects alpha 0.05 - adaptive grid",
  evidenceSynthesisSource = evidenceSynthesisSourceSccs
)
evidenceSynthesisAnalysisList <- list(metaAnalysisCm, metaAnalysisSccs)
evidenceSynthesisAnalysisSpecifications <- esModuleSettingsCreator$createModuleSpecifications(
  evidenceSynthesisAnalysisList
)
# PatientLevelPrediction -------------------------------
plpModuleSettingsCreator <- PatientLevelPredictionModule$new()
makeModelDesignSettings <- function(targetId, outcomeId, popSettings, covarSettings) {
  invisible(PatientLevelPrediction::createModelDesign(
    targetId = targetId,
    outcomeId = outcomeId,
    restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
    populationSettings = popSettings,
    covariateSettings = covarSettings,
    preprocessSettings = PatientLevelPrediction::createPreprocessSettings(),
    modelSettings = PatientLevelPrediction::setLassoLogisticRegression(),
    splitSettings = PatientLevelPrediction::createDefaultSplitSetting(),
    runCovariateSummary = T
  ))
}

plpPopulationSettings <- PatientLevelPrediction::createStudyPopulationSettings(
  startAnchor = "cohort start",
  riskWindowStart = 1,
  endAnchor = "cohort start",
  riskWindowEnd = 365,
  minTimeAtRisk = 1
)
plpCovarSettings <- FeatureExtraction::createDefaultCovariateSettings()

modelDesignList <- list()
for (i in 1:length(c(1002,1003))) {
  for (j in 1:length(outcomeOfInterestIds)) {
    modelDesignList <- append(
      modelDesignList,
      list(
        makeModelDesignSettings(
          targetId = c(1002,1003)[i],
          outcomeId = outcomeOfInterestIds[j],
          popSettings = plpPopulationSettings,
          covarSettings = plpCovarSettings
        )
      )
    )
  }
}

plpModuleSpecifications <- plpModuleSettingsCreator$createModuleSpecifications(
  modelDesignList = modelDesignList
)

# SelfControlledCaseSeries -------------------------------
sccsModuleSettingsCreator <- SelfControlledCaseSeriesModule$new()

exposuresOutcomeList <- list()
for (exposureOfInterestId in exposureOfInterestIds) {
  for (outcomeOfInterestId in outcomeOfInterestIds) {
    exposuresOutcomeList[[length(exposuresOutcomeList) + 1]] <- SelfControlledCaseSeries::createExposuresOutcome(
      outcomeId = outcomeOfInterestId,
      exposures = list(SelfControlledCaseSeries::createExposure(exposureId = exposureOfInterestId))
    )
  }
  for (negativeControlOutcomeId in negativeControlOutcomeIds) {
    exposuresOutcomeList[[length(exposuresOutcomeList) + 1]] <- SelfControlledCaseSeries::createExposuresOutcome(
      outcomeId = negativeControlOutcomeId,
      exposures = list(SelfControlledCaseSeries::createExposure(exposureId = exposureOfInterestId, trueEffectSize = 1))
    )
  }
}

getDbSccsDataArgs <- SelfControlledCaseSeries::createGetDbSccsDataArgs(
  maxCasesPerOutcome = 1e6,
  deleteCovariatesSmallCount = 0
)

createStudyPopulationArgs <- SelfControlledCaseSeries::createCreateStudyPopulationArgs(
  minAge = 18,
  naivePeriod = 365
)

covarPreExp <- SelfControlledCaseSeries::createEraCovariateSettings(
  label = "Pre-exposure",
  includeEraIds = "exposureId",
  start = -30,
  end = -1,
  endAnchor = "era start"
)

covarExposureOfInt <- SelfControlledCaseSeries::createEraCovariateSettings(
  label = "Main",
  includeEraIds = "exposureId",
  start = 0,
  startAnchor = "era start",
  end = 365, # set this to 30 or 180 from era start?
  endAnchor = "era end", # set to era start?
  profileLikelihood = TRUE,
  exposureOfInterest = TRUE
)

calendarTimeSettings <- SelfControlledCaseSeries::createCalendarTimeCovariateSettings(
  calendarTimeKnots = 5,
  allowRegularization = TRUE,
  computeConfidenceIntervals = FALSE
)

seasonalitySettings <- SelfControlledCaseSeries::createSeasonalityCovariateSettings(
  seasonKnots = 5,
  allowRegularization = TRUE,
  computeConfidenceIntervals = FALSE
)

createSccsIntervalDataArgs <- SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(covarPreExp, covarExposureOfInt),
  seasonalityCovariateSettings = seasonalitySettings,
  calendarTimeCovariateSettings = calendarTimeSettings,
  minCasesForTimeCovariates = 100000
)

fitSccsModelArgs <- SelfControlledCaseSeries::createFitSccsModelArgs(
  control = Cyclops::createControl(
    cvType = "auto",
    selectorType = "byPid",
    startingVariance = 0.1,
    seed = 1,
    resetCoefficients = TRUE,
    noiseLevel = "quiet"
  )
)

sccsAnalysis1 <- SelfControlledCaseSeries::createSccsAnalysis(
  analysisId = 1,
  description = "SCCS 18+",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs,
  fitSccsModelArgs = fitSccsModelArgs
)

sccsAnalysisList <- list(sccsAnalysis1)

sccsAnalysesSpecifications <- SelfControlledCaseSeries::createSccsAnalysesSpecifications(
  sccsAnalysisList = sccsAnalysisList,
  exposuresOutcomeList = exposuresOutcomeList
)

sccsModuleSpecifications <- sccsModuleSettingsCreator$createModuleSpecifications(
  sccsAnalysesSpecifications = sccsAnalysesSpecifications$toList()
)


# Create analysis specifications CDM modules ---------------
cdmModulesAnalysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortSharedResourcesSpecifications) |>
  addSharedResources(ncoCohortSharedResourceSpecifications) |>
  addCohortGeneratorModuleSpecifications(cgModuleSpecifications) |>
  addCohortDiagnosticsModuleSpecifications(cdModuleSpecifications) |>
  addCharacterizationModuleSpecifications(cModuleSpecifications) |>
  addCohortIncidenceModuleSpecifications(ciModuleSpecifications) |>
  addTreatmentPatternsModuleSpecifications(tpModuleSpecifications) |>
  addCohortMethodeModuleSpecifications(cmModuleSpecifications) |>
  addSelfControlledCaseSeriesModuleSpecifications(sccsModuleSpecifications) |>
  addPatientLevelPredictionModuleSpecifications(plpModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  object = cdmModulesAnalysisSpecifications,
  fileName = "inst/testdata/cdmModulesAnalysisSpecifications.json"
)

# Create analysis specifications results modules ---------------
cdmModulesAnalysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addEvidenceSynthesisModuleSpecifications(evidenceSynthesisAnalysisSpecifications)

ParallelLogger::saveSettingsToJson(
  object = cdmModulesAnalysisSpecifications,
  fileName = "inst/testdata/resultsModulesAnalysisSpecifications.json"
)

