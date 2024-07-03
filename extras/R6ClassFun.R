library(Strategus)
library(CohortGenerator)

# Setup some test data ------------
cohortDefinitionSet <- getCohortDefinitionSet(
  settingsFileName = "testdata/Cohorts.csv",
  jsonFolder = "testdata/cohorts",
  sqlFolder = "testdata/sql",
  packageName = "Strategus"
)
subsetOperations <- list(
  createDemographicSubset(
    name = "Demographic Criteria",
    ageMin = 18,
    ageMax = 64
  )
)
subsetDef <- createCohortSubsetDefinition(
  name = "test definition",
  definitionId = 1,
  subsetOperators = subsetOperations
)
cohortDefinitionSet <- cohortDefinitionSet |>
  addCohortSubsetDefinition(subsetDef)

ncoCohortSet <- readCsv(file = system.file("testdata/negative_controls_concept_set.csv",
                                           package = "Strategus"
))

# Create the analysis settings ---------------

# Cohort Generator -----------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Create the settings & validate them
cohortSharedResourcesSpecifications <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
cgModuleSettingsCreator$validateCohortSharedResourceSpecifications(cohortSharedResourcesSpecifications)

ncoCohortSharedResourceSpecifications <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSet, "first", TRUE)
cgModuleSettingsCreator$validateNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSharedResourceSpecifications)

cgModuleSettings <- cgModuleSettingsCreator$createModuleSpecifications()
cgModuleSettingsCreator$validateModuleSpecifications(cgModuleSettings)

# Characterization -------------------------------
cModuleSettingsCreator <- CharacterizationModule$new()
cModuleSpecifications <- cModuleSettingsCreator$createModuleSpecifications(
  targetIds = c(1, 2),
  outcomeIds = 3
)

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
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  incremental = FALSE
)

# Cohort Incidence -----------------
library(CohortIncidence)
ciModuleSettingsCreator <- CohortIncidenceModule$new()
targets <- list(
  createCohortRef(id = 1, name = "Celecoxib"),
  createCohortRef(id = 2, name = "Diclofenac"),
  createCohortRef(id = 4, name = "Celecoxib Age >= 30"),
  createCohortRef(id = 5, name = "Diclofenac Age >= 30")
)
outcomes <- list(createOutcomeDef(id = 1, name = "GI bleed", cohortId = 3, cleanWindow = 9999))

tars <- list(
  createTimeAtRiskDef(id = 1, startWith = "start", endWith = "end"),
  createTimeAtRiskDef(id = 2, startWith = "start", endWith = "start", endOffset = 365)
)
analysis1 <- createIncidenceAnalysis(
  targets = c(1, 2, 4, 5),
  outcomes = c(1),
  tars = c(1, 2)
)

irDesign <- createIncidenceDesign(
  targetDefs = targets,
  outcomeDefs = outcomes,
  tars = tars,
  analysisList = list(analysis1),
  strataSettings = createStrataSettings(
    byYear = TRUE,
    byGender = TRUE
  )
)

ciModuleSettings <- ciModuleSettingsCreator$createModuleSpecifications(
  irDesign = irDesign$toList()
)
ciModuleSettingsCreator$validateModuleSpecifications(ciModuleSettings)

# Cohort Method ----------------------
library(CohortMethod)
cmModuleSettingsCreator <- CohortMethodModule$new()
negativeControlOutcomes <- lapply(
  X = ncoCohortSet$cohortId,
  FUN = createOutcome,
  outcomeOfInterest = FALSE,
  trueEffectSize = 1,
  priorOutcomeLookback = 30
)

outcomesOfInterest <- lapply(
  X = 3,
  FUN = createOutcome,
  outcomeOfInterest = TRUE
)

outcomes <- append(
  negativeControlOutcomes,
  outcomesOfInterest
)

tcos1 <- CohortMethod::createTargetComparatorOutcomes(
  targetId = 1,
  comparatorId = 2,
  outcomes = outcomes,
  excludedCovariateConceptIds = c(1118084, 1124300)
)
tcos2 <- CohortMethod::createTargetComparatorOutcomes(
  targetId = 4,
  comparatorId = 5,
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
  riskWindowEnd = 30,
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
  analysesToExclude = analysesToExclude
)

# SelfControlledCaseSeries -------------------------------
library(SelfControlledCaseSeries)
sccsModuleSettingsCreator <- SelfControlledCaseSeriesModule$new()

# Exposures-outcomes
negativeControlOutcomeIds <- ncoCohortSet$cohortId
outcomeOfInterestIds <- c(3)
exposureOfInterestIds <- c(1, 2)

exposuresOutcomeList <- list()
for (exposureOfInterestId in exposureOfInterestIds) {
  for (outcomeOfInterestId in outcomeOfInterestIds) {
    exposuresOutcomeList[[length(exposuresOutcomeList) + 1]] <- createExposuresOutcome(
      outcomeId = outcomeOfInterestId,
      exposures = list(createExposure(exposureId = exposureOfInterestId))
    )
  }
  for (negativeControlOutcomeId in negativeControlOutcomeIds) {
    exposuresOutcomeList[[length(exposuresOutcomeList) + 1]] <- createExposuresOutcome(
      outcomeId = negativeControlOutcomeId,
      exposures = list(createExposure(exposureId = exposureOfInterestId, trueEffectSize = 1))
    )
  }
}

# Analysis settings ------------------------------------------------------------
getDbSccsDataArgs <- SelfControlledCaseSeries::createGetDbSccsDataArgs(
  studyStartDate = "",
  studyEndDate = "",
  maxCasesPerOutcome = 1e6,
  useNestingCohort = TRUE,
  nestingCohortId = 1,
  deleteCovariatesSmallCount = 0
)

createStudyPopulation6AndOlderArgs <- SelfControlledCaseSeries::createCreateStudyPopulationArgs(
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
  end = 0,
  endAnchor = "era end",
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
  description = "SCCS age 18-",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulation6AndOlderArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs,
  fitSccsModelArgs = fitSccsModelArgs
)

sccsAnalysisList <- list(sccsAnalysis1)

# SCCS module specs ------------------------------------------------------------
sccsModuleSpecifications <- sccsModuleSettingsCreator$createModuleSpecifications(
  sccsAnalysisList = sccsAnalysisList,
  exposuresOutcomeList = exposuresOutcomeList,
  combineDataFetchAcrossOutcomes = FALSE
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
for (i in 1:length(exposureOfInterestIds)) {
  for (j in 1:length(outcomeOfInterestIds)) {
    modelDesignList <- append(
      modelDesignList,
      list(
        makeModelDesignSettings(
          targetId = exposureOfInterestIds[i],
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




# Create analysis specifications ---------------
analysisSpecifications <- createEmptyAnalysisSpecificiations() |>
  addSharedResources(cohortSharedResourcesSpecifications) |>
  addSharedResources(ncoCohortSharedResourceSpecifications) |>
  addModuleSpecifications(cgModuleSettings) |>
  addModuleSpecifications(cdModuleSpecifications) |>
  addModuleSpecifications(cmModuleSpecifications) |>
  addModuleSpecifications(sccsModuleSpecifications)
  # NOT WORKING
  #addModuleSpecifications(cModuleSpecifications) |>
  #addModuleSpecifications(plpModuleSpecifications)
  # MOSTLY WORKING
  #addModuleSpecifications(ciModuleSettings) |>

# Cleanup any prior results -----------------
outputFolder <- "D:/TEMP/StrategusR6Testing"
unlink(outputFolder, recursive = T, force = T)
dir.create(outputFolder, recursive = T)

# Execute -------------------
ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path(outputFolder, "analysisSettings.json"))
workFolder <- file.path(outputFolder, "work_folder")
resultsFolder <- file.path(outputFolder, "results_folder")

executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "strategus_test"),
  workFolder = workFolder,
  resultsFolder = resultsFolder,
  minCellCount = 5
)

connectionDetails <- Eunomia::getEunomiaConnectionDetails(
  databaseFile = file.path(outputFolder, "Eunomia.sqlite"),
  overwrite = TRUE
)
#debugonce(Strategus::execute)
#debugonce(CohortDiagnostics:::computeCohortCounts)
Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  connectionDetails = connectionDetails
)

# # # DEBUG CD
# cdModule <- CohortDiagnosticsModule$new()
# debugonce(cdModule$execute)
# executionSettings$databaseId = "Eunomia"
# cdModule$execute(
#   analysisSpecifications = analysisSpecifications,
#   executionSettings = executionSettings,
#   connectionDetails = connectionDetails
# )

# Create empty results database -------------------------
library(RSQLite)
mydb <- dbConnect(RSQLite::SQLite(), file.path(outputFolder, "results.sqlite"))
dbDisconnect(mydb)

resultsConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = file.path(outputFolder, "results.sqlite")
)

# Create results schema tables -------------------------
# NOTE: resultsExecutionSettings uses a diff. folder to hold
# any/all results schema creation results
resultsExecutionSettings <- Strategus::createResultsExecutionSettings(
  resultsDatabaseSchema = "main",
  resultsFolder = executionSettings$resultsFolder,
  workFolder = file.path(outputFolder, "schema_creation", "work_folder")
)

# NOTE: CI has not implemented this so it will error out.
Strategus::createResultDataModels(
  analysisSpecifications = analysisSpecifications,
  resultsExecutionSettings = resultsExecutionSettings,
  resultsConnectionDetails = resultsConnectionDetails
)

# Upload results ---------------
resultsExecutionSettings <- Strategus::createResultsExecutionSettings(
  resultsDatabaseSchema = "main",
  workFolder = executionSettings$workFolder,
  resultsFolder = executionSettings$resultsFolder
)

#debugonce(Strategus::uploadResults)
Strategus::uploadResults(
  analysisSpecifications = analysisSpecifications,
  resultsExecutionSettings = resultsExecutionSettings,
  resultsConnectionDetails = resultsConnectionDetails
)

# Peek in the results database ---------------
# conn <- DatabaseConnector::connect(resultsConnectionDetails)
# DatabaseConnector::disconnect(conn)

# Run EvidenceSythesis Module ------------------
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
esAnalysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addModuleSpecifications(evidenceSynthesisAnalysisSpecifications)

ParallelLogger::saveSettingsToJson(esAnalysisSpecifications, file.path(outputFolder, "evidenceSynthesisAnalysisSpecifications.json"))

debugonce(Strategus::execute)
Strategus::execute(
  analysisSpecifications = esAnalysisSpecifications,
  executionSettings = resultsExecutionSettings,
  connectionDetails = resultsConnectionDetails
)


# Review results --------------------------
library(ShinyAppBuilder)
library(OhdsiShinyModules)
# ADD OR REMOVE MODULES TAILORED TO YOUR STUDY
shinyConfig <- initializeModuleConfig() |>
  addModuleConfig(
    createDefaultAboutConfig()
  )  |>
  addModuleConfig(
    createDefaultDatasourcesConfig()
  )  |>
  addModuleConfig(
    createDefaultCohortGeneratorConfig()
  ) |>
  addModuleConfig(
    createDefaultCohortDiagnosticsConfig()
  ) |>
  # addModuleConfig(
  #   createDefaultCharacterizationConfig()
  # ) |>
  # addModuleConfig(
  #   createDefaultPredictionConfig()
  # ) |>
  addModuleConfig(
   createDefaultCohortMethodConfig()
  )
  # addModuleConfig(
  #   createDefaultSccsConfig()
  # ) |>
  # addModuleConfig(
  #   createDefaultEvidenceSynthesisConfig()
  # )

# now create the shiny app based on the config file and view the results
# based on the connection
ShinyAppBuilder::createShinyApp(
  config = shinyConfig,
  connectionDetails = resultsConnectionDetails,
  resultDatabaseSettings = createDefaultResultDatabaseSettings(schema = "main")
)
