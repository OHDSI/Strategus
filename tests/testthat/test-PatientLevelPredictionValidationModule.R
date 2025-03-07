library(testthat)
library(dplyr)
library(Strategus)

test_that("Test PLP Validation Module", {
  workFolder <- tempfile("work")
  dir.create(workFolder)
  resultsFolder <- tempfile("results")
  dir.create(resultsFolder)
  workFolder2 <- tempfile("work2")
  dir.create(workFolder2)
  resultsFolder2 <- tempfile("results2")
  dir.create(resultsFolder2)
  withr::defer(
    {
      unlink(workFolder, recursive = TRUE, force = TRUE)
      unlink(resultsFolder, recursive = TRUE, force = TRUE)
      unlink(workFolder2, recursive = TRUE, force = TRUE)
      unlink(resultsFolder2, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )

  # create cohorts in Eunomia
  cohortDefinitionSet <- Eunomia::createCohorts(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortDatabaseSchema = workDatabaseSchema
  )

  cohortDefinitionSet <- cohortDefinitionSet %>% dplyr::rename(
    cohortName = "name"
  ) %>%
    mutate(
      json = '{
  "ConceptSets": [
    {
      "id": 2,
      "name": "Inpatient/ER visit",
      "expression": {
        "items": [
          {
            "concept": {
              "CONCEPT_CLASS_ID": "Visit",
              "CONCEPT_CODE": "ERIP",
              "CONCEPT_ID": 262,
              "CONCEPT_NAME": "Emergency Room and Inpatient Visit",
              "DOMAIN_ID": "Visit",
              "INVALID_REASON": "V",
              "INVALID_REASON_CAPTION": "Valid",
              "STANDARD_CONCEPT": "S",
              "STANDARD_CONCEPT_CAPTION": "Standard",
              "VOCABULARY_ID": "Visit"
            },
            "includeDescendants": true
          },
          {
            "concept": {
              "CONCEPT_CLASS_ID": "Visit",
              "CONCEPT_CODE": "IP",
              "CONCEPT_ID": 9201,
              "CONCEPT_NAME": "Inpatient Visit",
              "DOMAIN_ID": "Visit",
              "INVALID_REASON": "V",
              "INVALID_REASON_CAPTION": "Valid",
              "STANDARD_CONCEPT": "S",
              "STANDARD_CONCEPT_CAPTION": "Standard",
              "VOCABULARY_ID": "Visit"
            },
            "includeDescendants": true
          },
          {
            "concept": {
              "CONCEPT_CLASS_ID": "Visit",
              "CONCEPT_CODE": "ER",
              "CONCEPT_ID": 9203,
              "CONCEPT_NAME": "Emergency Room Visit",
              "DOMAIN_ID": "Visit",
              "INVALID_REASON": "V",
              "INVALID_REASON_CAPTION": "Valid",
              "STANDARD_CONCEPT": "S",
              "STANDARD_CONCEPT_CAPTION": "Standard",
              "VOCABULARY_ID": "Visit"
            },
            "includeDescendants": true
          }
        ]
      }
    }
  ],
  "PrimaryCriteria": {
    "CriteriaList": [
      {
        "VisitOccurrence": {
          "CodesetId": 2
        }
      }
    ],
    "ObservationWindow": {
      "PriorDays": 0,
      "PostDays": 0
    },
    "PrimaryCriteriaLimit": {
      "Type": "All"
    }
  },
  "QualifiedLimit": {
    "Type": "First"
  },
  "ExpressionLimit": {
    "Type": "All"
  },
  "InclusionRules": [],
  "EndStrategy": {
    "DateOffset": {
      "DateField": "EndDate",
      "Offset": 0
    }
  },
  "CensoringCriteria": [],
  "CollapseSettings": {
    "CollapseType": "ERA",
    "EraPad": 1
  },
  "CensorWindow": {},
  "cdmVersionRange": ">=5.0.0"
}',
      sql = ''
    )

  # add the cohortDefSet into shared resource
  cohortGeneratorModule <- Strategus::CohortGeneratorModule$new()
  cohortDefinitionShared <- cohortGeneratorModule$createCohortSharedResourceSpecifications(cohortDefinitionSet)

  # add model to folder
  plpModel <-  PatientLevelPrediction::loadPlpModel(system.file("testdata/plpvmodule", package = "Strategus")) #readRDS("tests/plpModel.rds")

  # Create the validation settings and run the module
  plpvSettingsCreator <- PatientLevelPredictionValidationModule$new()
  plpModuleSettings <- plpvSettingsCreator$createModuleSpecifications(
    validationList = list(
      PatientLevelPrediction::createValidationDesign(
        targetId = plpModel$modelDesign$targetId,
        outcomeId = plpModel$modelDesign$outcomeId,
        plpModelList = list(plpModel)
      )
    )
  )

  analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
    Strategus::addSharedResources(cohortDefinitionShared) %>%
    addModuleSpecifications(plpModuleSettings)

  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
    workFolder = workFolder,
    resultsFolder = resultsFolder
  )

  Strategus::execute(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    connectionDetails = connectionDetails
  )

  # check the csv files are all there:
  testthat::expect_true(dir.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','cohorts.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','cohort_definition.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','database_meta_data.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','database_details.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','tars.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','tidy_covariates_settings.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','sample_settings.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','plp_data_settings.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','split_settings.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','feature_engineering_settings.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','population_settings.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','covariate_settings.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','model_settings.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','model_designs.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','models.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','performances.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','recalibrations.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','attrition.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','evaluation_statistics.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','threshold_summary.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','prediction_distribution.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','demographic_summary.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','covariate_summary.csv')))
  testthat::expect_true(file.exists(file.path(resultsFolder,'PatientLevelPredictionValidationModule','calibration_summary.csv')))

  # check using model in package
  createPackageModel <- function(modelFolder, package){
    result <- list(
      type = 'package',
      modelFolder = modelFolder,
      package = package
    )
    class(result) <- 'plpModel'

    return(result)
  }

  # TESTING MODEL INSIDE PACKAGE
  # Create the validation settings and run the module
  plpvSettingsCreator <- PatientLevelPredictionValidationModule$new()
  plpModuleSettings <- plpvSettingsCreator$createModuleSpecifications(
    validationList = PatientLevelPrediction::createValidationDesign(
        targetId = 1,
        outcomeId = 3,
      plpModelList = list(createPackageModel(
        package = 'Strategus',
        modelFolder = 'testdata/plpvmodule'
      ))
      )
  )

  analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
    Strategus::addSharedResources(cohortDefinitionShared) %>%
    addModuleSpecifications(plpModuleSettings)

  executionSettings <- createCdmExecutionSettings(
    workDatabaseSchema = workDatabaseSchema,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
    workFolder = workFolder2,
    resultsFolder = resultsFolder2
  )

  Strategus::execute(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    connectionDetails = connectionDetails
  )

})
