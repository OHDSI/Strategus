library(testthat)
library(dplyr)

# test_that("TreatmentPatterns Backwards Compatibility ", {
#   skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))

#   tempDir <- file.path(tempdir(), "Strategus-TP")
#   on.exit(unlink(tempDir, recursive = TRUE))

#   testSettings <- generateCohortTable()
#   tp <- Strategus::TreatmentPatternsModule$new()

#   modSpec <- tp$createModuleSpecifications(
#     cohorts = testSettings$cohorts,
#     includeTreatments = "startDate",
#     indexDateOffset = 0,
#     minEraDuration = 7,
#     splitEventCohorts = NULL,
#     splitTime = NULL,
#     eraCollapseSize = 14,
#     combinationWindow = 7,
#     minPostCombinationDuration = 7,
#     filterTreatments = "First",
#     maxPathLength = 5
#   )

#   analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
#     Strategus::addTreatmentPatternsModuleSpecifications(modSpec)

#   executionSettings <- Strategus::createCdmExecutionSettings(
#     workDatabaseSchema = testSettings$resultSchema,
#     cdmDatabaseSchema = testSettings$cdmSchema,
#     cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort_table"),
#     tempEmulationSchema = NULL,
#     workFolder = file.path(tempDir, "work_folder"),
#     resultsFolder = file.path(tempDir, "results_folder"),
#     logFileName = "log.txt",
#     minCellCount = 5,
#     incremental = FALSE,
#     maxCores = 1
#   )

#   Strategus::execute(
#     connectionDetails = testSettings$connectionDetails,
#     analysisSpecifications = analysisSpecifications,
#     executionSettings = executionSettings
#   )


#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "attrition.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_age.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_sex.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_year.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "metadata.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "summary_event_duration.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "treatment_pathways.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "resultsDataModelSpecification.csv")))
# })

# test_that("TreatmentPatterns execute single analysis", {
#   skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))

#   tempDir <- file.path(tempdir(), "Strategus-TP")
#   on.exit(unlink(tempDir, recursive = TRUE))

#   testSettings <- generateCohortTable()

#   tp <- Strategus::TreatmentPatternsModule$new()

#   modSpec <- tp$createModuleSpecifications(
#     cohorts = testSettings$cohorts,
#     startAnchor = "startDate",
#     windowStart = 0,
#     endAnchor = "endDate",
#     windowEnd = 0,
#     minEraDuration = 7,
#     splitEventCohorts = NULL,
#     splitTime = NULL,
#     eraCollapseSize = 14,
#     combinationWindow = 7,
#     minPostCombinationDuration = 7,
#     filterTreatments = "First",
#     maxPathLength = 5
#   )

#   analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
#     Strategus::addTreatmentPatternsModuleSpecifications(modSpec)

#   workFolder <- file.path(tempDir, "work_folder")

#   executionSettings <- Strategus::createCdmExecutionSettings(
#     workDatabaseSchema = testSettings$resultSchema,
#     cdmDatabaseSchema = testSettings$cdmSchema,
#     cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort_table"),
#     tempEmulationSchema = NULL,
#     workFolder = workFolder,
#     resultsFolder = file.path(tempDir, "results_folder"),
#     logFileName = "log.txt",
#     minCellCount = 5,
#     incremental = FALSE,
#     maxCores = 1
#   )

#   Strategus::execute(
#     connectionDetails = testSettings$connectionDetails,
#     analysisSpecifications = analysisSpecifications,
#     executionSettings = executionSettings
#   )

#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "attrition.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_age.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_sex.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_year.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "metadata.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "summary_event_duration.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "treatment_pathways.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "resultsDataModelSpecification.csv")))
#   expect_true(file.exists(file.path(workFolder, "TreatmentPatternsModule/", "passed_pathway_runs.csv")))
# })

test_that("Bad Cohort Names", {
  skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))
  tp <- Strategus::TreatmentPatternsModule$new()


  testTable1 <- data.frame(
    cohortId = c(8,1,3,0),
    cohortName = c("Viral+Sinusitis", "+Acetaminophen", "Aspirin-", "Death"),
    type = c("target", "event", "event", "exit")
  )

  expect_error(
    modSpecs <- list(
      tp$createModuleSpecifications(
        analysisId = 8,
        cohorts = testTable1,
        startAnchor = "startDate",
        windowStart = 0,
        endAnchor = "endDate",
        windowEnd = 0,
        minEraDuration = 7,
        splitEventCohorts = NULL,
        splitTime = NULL,
        eraCollapseSize = 14,
        combinationWindow = 7,
        minPostCombinationDuration = 7,
        filterTreatments = "First",
        maxPathLength = 5
      )
    ),
    "Invalid target cohort name 'Viral\\+Sinusitis': Invalid target cohort name; remove -/\\+"
  )
})

test_that("Unique Analysis Ids", {
  skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))

  tempDir <- file.path(tempdir(), "Strategus-TP")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()
  tp <- Strategus::TreatmentPatternsModule$new()

  modSpecs <- list(
    tp$createModuleSpecifications(
      cohorts = testSettings$cohorts,
      startAnchor = "startDate",
      windowStart = 0,
      endAnchor = "endDate",
      windowEnd = 0,
      minEraDuration = 7,
      splitEventCohorts = NULL,
      splitTime = NULL,
      eraCollapseSize = 14,
      combinationWindow = 7,
      minPostCombinationDuration = 7,
      filterTreatments = "First",
      maxPathLength = 5
    ),
    tp$createModuleSpecifications(
      cohorts = testSettings$cohorts,
      startAnchor = "startDate",
      windowStart = 0,
      endAnchor = "endDate",
      windowEnd = 0,
      minEraDuration = 7,
      splitEventCohorts = NULL,
      splitTime = NULL,
      eraCollapseSize = 14,
      combinationWindow = 7,
      minPostCombinationDuration = 7,
      filterTreatments = "First",
      maxPathLength = 5
    )
  )
  expect_error(
    tp$createMultiAnalysisModuleSpecification(tpAnalysisList = modSpecs),
    "Each analysis need unique id"
  )

})

# test_that("TreatmentPatterns: execute method with multiple analysis", {
#   skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))

#   tempDir <- file.path(tempdir(), "Strategus-TP")
#   on.exit(unlink(tempDir, recursive = TRUE))

#   testSettings <- generateCohortTable()
#   tp <- Strategus::TreatmentPatternsModule$new()

#   modSpecs <- list(
#     tp$createModuleSpecifications(
#       analysisId = 8,
#       cohorts = testSettings$cohorts,
#       startAnchor = "startDate",
#       windowStart = 0,
#       endAnchor = "endDate",
#       windowEnd = 0,
#       minEraDuration = 7,
#       splitEventCohorts = NULL,
#       splitTime = NULL,
#       eraCollapseSize = 14,
#       combinationWindow = 7,
#       minPostCombinationDuration = 7,
#       filterTreatments = "First",
#       maxPathLength = 5
#     ),
#     tp$createModuleSpecifications(
#       analysisId = 10,
#       cohorts = testSettings$cohorts,
#       includeTreatments = "startDate",
#       indexDateOffset = 0,
#       minEraDuration = 7,
#       splitEventCohorts = NULL,
#       splitTime = NULL,
#       eraCollapseSize = 14,
#       combinationWindow = 7,
#       minPostCombinationDuration = 7,
#       filterTreatments = "First",
#       maxPathLength = 5
#     )
#   )

#   specifications <- tp$createMultiAnalysisModuleSpecification(tpAnalysisList = modSpecs)

#   analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
#     Strategus::addTreatmentPatternsModuleSpecifications(specifications)

#   executionSettings <- Strategus::createCdmExecutionSettings(
#     workDatabaseSchema = testSettings$resultSchema,
#     cdmDatabaseSchema = testSettings$cdmSchema,
#     cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort_table"),
#     tempEmulationSchema = NULL,
#     workFolder = file.path(tempDir, "work_folder"),
#     resultsFolder = file.path(tempDir, "results_folder"),
#     logFileName = "log.txt",
#     minCellCount = 5,
#     incremental = FALSE,
#     maxCores = 1
#   )

#   Strategus::execute(
#     connectionDetails = testSettings$connectionDetails,
#     analysisSpecifications = analysisSpecifications,
#     executionSettings = executionSettings
#   )


#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "attrition.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_age.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_sex.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "counts_year.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "metadata.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "summary_event_duration.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "treatment_pathways.csv"))))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, "resultsDataModelSpecification.csv")))
#   expect_true(file.exists(file.path(executionSettings$resultsFolder, tp$moduleName, paste0(tp$tablePrefix, "analysis_cohorts.csv"))))
# })

test_that("TreatmentPatterns: pathway failed", {
  skip_if(!require("TreatmentPatterns", quietly = TRUE, character.only = TRUE, warn.conflicts = FALSE))

  tempDir <- file.path(tempdir(), "Strategus-TP")
  on.exit(unlink(tempDir, recursive = TRUE))

  testSettings <- generateCohortTable()
  tp <- Strategus::TreatmentPatternsModule$new()

  modSpecs <- list(
    tp$createModuleSpecifications(
      analysisId = 8,
      cohorts = testSettings$cohorts,
      startAnchor = "startDate",
      windowStart = 0,
      endAnchor = "endDate",
      windowEnd = 0,
      minEraDuration = 7,
      splitEventCohorts = NULL,
      splitTime = NULL,
      eraCollapseSize = 14,
      combinationWindow = 7,
      minPostCombinationDuration = 7,
      filterTreatments = "First",
      maxPathLength = 5
    )
  )


  # install mock into the package namespace (bare name computePathways; NO backticks or pkg:: prefix)
  testthat::local_mocked_bindings(
    computePathways = function(...) stop("forced computePathways failure"),
    .package = "TreatmentPatterns"
  )

  workFolder <- file.path(tempDir, "work_folder")

  specifications <- tp$createMultiAnalysisModuleSpecification(tpAnalysisList = modSpecs)

  analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
    Strategus::addTreatmentPatternsModuleSpecifications(specifications)

  executionSettings <- Strategus::createCdmExecutionSettings(
    workDatabaseSchema = testSettings$resultSchema,
    cdmDatabaseSchema = testSettings$cdmSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort_table"),
    tempEmulationSchema = NULL,
    workFolder = workFolder,
    resultsFolder = file.path(tempDir, "results_folder"),
    logFileName = "log.txt",
    minCellCount = 5,
    incremental = FALSE,
    maxCores = 1
  )

  Strategus::execute(
    connectionDetails = testSettings$connectionDetails,
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings
  )

  expect_true(file.exists(file.path(workFolder, "TreatmentPatternsModule/", "failed_pathway_runs.csv")))
})
