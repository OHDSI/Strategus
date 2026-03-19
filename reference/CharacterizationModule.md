# Characterize cohorts with the [HADES Characterization Package](https://ohdsi.github.io/Characterization/)

Computes cohort characterization information against the OMOP Common
Data Model

## Super class

[`Strategus::StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `CharacterizationModule`

## Public fields

- `tablePrefix`:

  The table prefix to append to the results tables

## Methods

### Public methods

- [`CharacterizationModule$new()`](#method-CharacterizationModule-new)

- [`CharacterizationModule$execute()`](#method-CharacterizationModule-execute)

- [`CharacterizationModule$createResultsDataModel()`](#method-CharacterizationModule-createResultsDataModel)

- [`CharacterizationModule$getResultsDataModelSpecification()`](#method-CharacterizationModule-getResultsDataModelSpecification)

- [`CharacterizationModule$uploadResults()`](#method-CharacterizationModule-uploadResults)

- [`CharacterizationModule$createModuleSpecifications()`](#method-CharacterizationModule-createModuleSpecifications)

- [`CharacterizationModule$clone()`](#method-CharacterizationModule-clone)

Inherited methods

- [`Strategus::StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`Strategus::StrategusModule$validateModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateModuleSpecifications)
- [`Strategus::StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    CharacterizationModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Execute characterization

#### Usage

    CharacterizationModule$execute(
      connectionDetails,
      analysisSpecifications,
      executionSettings
    )

#### Arguments

- `connectionDetails`:

  An object of class `connectionDetails` as created by the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- `analysisSpecifications`:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- `analysisSpecifications`:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- `executionSettings`:

  An object of type `ExecutionSettings` as created by
  [`createCdmExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createCdmExecutionSettings.md)
  or
  [`createResultsExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsExecutionSettings.md).

------------------------------------------------------------------------

### Method `createResultsDataModel()`

Create the results data model for the module

#### Usage

    CharacterizationModule$createResultsDataModel(
      resultsConnectionDetails,
      resultsDatabaseSchema,
      tablePrefix = self$tablePrefix
    )

#### Arguments

- `resultsConnectionDetails`:

  The connection details to the results database which is an object of
  class `connectionDetails` as created by the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- `resultsConnectionDetails`:

  The connection details to the results database which is an object of
  class `connectionDetails` as created by the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- `resultsDatabaseSchema`:

  The schema in the results database that holds the results data model.

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method `getResultsDataModelSpecification()`

Get the results data model specification for the module

#### Usage

    CharacterizationModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for the module

#### Usage

    CharacterizationModule$uploadResults(
      resultsConnectionDetails,
      analysisSpecifications,
      resultsDataModelSettings
    )

#### Arguments

- `resultsConnectionDetails`:

  The connection details to the results database which is an object of
  class `connectionDetails` as created by the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- `resultsConnectionDetails`:

  The connection details to the results database which is an object of
  class `connectionDetails` as created by the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- `analysisSpecifications`:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- `analysisSpecifications`:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- `resultsDataModelSettings`:

  The results data model settings as created using \[@seealso
  [`createResultsDataModelSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsDataModelSettings.md)\]

------------------------------------------------------------------------

### Method `createModuleSpecifications()`

Creates the CharacterizationModule Specifications

#### Usage

    CharacterizationModule$createModuleSpecifications(
      targetIds,
      limitToFirstInNDays = rep(99999, length(targetIds)),
      minPriorObservation = 365,
      outcomeIds,
      outcomeWashoutDays = c(365),
      riskWindowStart = c(1, 1),
      startAnchor = c("cohort start", "cohort start"),
      riskWindowEnd = c(0, 365),
      endAnchor = c("cohort end", "cohort end"),
      dechallengeStopInterval = 30,
      dechallengeEvaluationWindow = 30,
      mode = "CohortIncidence",
      minSMD = 0.01,
      minCharacterizationMean = 0.01,
      minCovariateCount = 5,
      covariateSettings = FeatureExtraction::createCovariateSettings(useDemographicsGender =
        T, useDemographicsAge = T, useDemographicsAgeGroup = T, useDemographicsRace = T,
        useDemographicsEthnicity = T, useDemographicsIndexYear = T, useDemographicsIndexMonth
        = T, useDemographicsTimeInCohort = T, useDemographicsPriorObservationTime = T,
        useDemographicsPostObservationTime = T, useConditionGroupEraLongTerm = T,
        useDrugGroupEraOverlapping = T, useDrugGroupEraLongTerm = T,
        useProcedureOccurrenceLongTerm = T, useMeasurementLongTerm = T,

        useObservationLongTerm = T, useDeviceExposureLongTerm = T,
        useVisitConceptCountLongTerm = T, useConditionGroupEraShortTerm = T,
        useDrugGroupEraShortTerm = T, useProcedureOccurrenceShortTerm = T,
        useMeasurementShortTerm = T, useObservationShortTerm = T, useDeviceExposureShortTerm
        = T, useVisitConceptCountShortTerm = T, endDays = 0, longTermStartDays = -365,
        shortTermStartDays = -30),
      caseCovariateSettings =
        Characterization::createDuringCovariateSettings(useConditionGroupEraDuring = T,
        useDrugGroupEraDuring = T, useProcedureOccurrenceDuring = T, useDeviceExposureDuring
        = T, useMeasurementDuring = T, useObservationDuring = T, useVisitConceptCountDuring =
        T),
      casePreTargetDuration = 365,
      casePostOutcomeDuration = 365,
      includeTimeToEvent = TRUE,
      includeDechallengeRechallenge = TRUE,
      includeTargetBaseline = TRUE,
      includeRiskFactors = TRUE,
      includeCaseSeries = TRUE,
      outputTable = "characterization_cohorts"
    )

#### Arguments

- `targetIds`:

  A vector of cohort IDs to use as the target(s) for the
  characterization

- `limitToFirstInNDays`:

  A vector of number of days of minimum between target exposures for
  each target (same length as the targetIds) - use 99999 to restrict to
  first exposure per person in target cohort

- `minPriorObservation`:

  The number of days of minimum observation a patient in the target
  populations must have

- `outcomeIds`:

  A vector of cohort IDs to use as the outcome(s) for the
  characterization

- `outcomeWashoutDays`:

  A vector of integers specifying the washout days for each outcome
  (same length as the outcomeIds)

- `riskWindowStart`:

  The number of days after start anchor to start the time-at-risk (can
  be a vector for multiple TARS)

- `startAnchor`:

  The TAR starts relative to this either cohort start or cohort end (can
  be a vector for multiple TARS)

- `riskWindowEnd`:

  The number of days after end anchor to end the time-at-risk (can be a
  vector for multiple TARS)

- `endAnchor`:

  The TAR ends relative to this either cohort start or cohort end (can
  be a vector for multiple TARS)

- `dechallengeStopInterval`:

  description

- `dechallengeEvaluationWindow`:

  description

- `mode`:

  Pick one of 'CohortIncidence'/'Efficient'/'PatientLevelPrediction' to
  specify how the non-cases are defined

- `minSMD`:

  The minimum standardized mean difference for the risk factors analysis

- `minCharacterizationMean`:

  The minimum fraction patients in the target have a covariate for it to
  be included

- `minCovariateCount`:

  The minimum number of patients in the analysis to have a covariate for
  it to be included

- `covariateSettings`:

  Covariates for the database, cohort and risk factor characterization

- `caseCovariateSettings`:

  Covariates for the case-series characterization

- `casePreTargetDuration`:

  The number of days before target start to use for case-series

- `casePostOutcomeDuration`:

  The number of days after outcome start to use for case-series

- `includeTimeToEvent`:

  Lets you skip running a time to event analyses when set to FALSE

- `includeDechallengeRechallenge`:

  Lets you skip running a dechallenge-rechallenge analyses when set to
  FALSE

- `includeTargetBaseline`:

  Lets you skip running the target baseline analyses when set to FALSE

- `includeRiskFactors`:

  Lets you skip running the risk factor analyses when set to FALSE

- `includeCaseSeries`:

  Lets you skip running the case series analyses when set to FALSE

- `outputTable`:

  The table used by characterization to create the cohorts used in
  characterization (included targets, cases, non-cases, etc.). The spec
  hash and database hash are added to this name to make it unique per
  run.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CharacterizationModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
