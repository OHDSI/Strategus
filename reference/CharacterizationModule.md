# Characterize cohorts with the [HADES Characterization Package](https://ohdsi.github.io/Characterization/)

Computes cohort characterization information against the OMOP Common
Data Model

## Super class

[`StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `CharacterizationModule`

## Public fields

- `tablePrefix`:

  The table prefix to append to the results tables

## Methods

### Public methods

- [`CharacterizationModule$new()`](#method-CharacterizationModule-initialize)

- [`CharacterizationModule$execute()`](#method-CharacterizationModule-execute)

- [`CharacterizationModule$createResultsDataModel()`](#method-CharacterizationModule-createResultsDataModel)

- [`CharacterizationModule$getResultsDataModelSpecification()`](#method-CharacterizationModule-getResultsDataModelSpecification)

- [`CharacterizationModule$uploadResults()`](#method-CharacterizationModule-uploadResults)

- [`CharacterizationModule$createModuleSpecifications()`](#method-CharacterizationModule-createModuleSpecifications)

- [`CharacterizationModule$clone()`](#method-CharacterizationModule-clone)

Inherited methods

- [`StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`StrategusModule$getPackageInformation()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-getPackageInformation)
- [`StrategusModule$validateModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateModuleSpecifications)
- [`StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### `CharacterizationModule$new()`

Initialize the module

#### Usage

    CharacterizationModule$new()

------------------------------------------------------------------------

### `CharacterizationModule$execute()`

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

### `CharacterizationModule$createResultsDataModel()`

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

### `CharacterizationModule$getResultsDataModelSpecification()`

Get the results data model specification for the module

#### Usage

    CharacterizationModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### `CharacterizationModule$uploadResults()`

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

  The results data model settings as created using
  [`createResultsDataModelSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsDataModelSettings.md).

------------------------------------------------------------------------

### `CharacterizationModule$createModuleSpecifications()`

Creates the CharacterizationModule Specifications

#### Usage

    CharacterizationModule$createModuleSpecifications(
      characterizationSettings = NULL,
      targetIds = NULL,
      limitToFirstInNDays = NULL,
      minPriorObservation = NULL,
      outcomeIds = NULL,
      outcomeWashoutDays = NULL,
      riskWindowStart = NULL,
      startAnchor = NULL,
      riskWindowEnd = NULL,
      endAnchor = NULL,
      dechallengeStopInterval = NULL,
      dechallengeEvaluationWindow = NULL,
      covariateSettings = NULL,
      caseCovariateSettings = NULL,
      casePreTargetDuration = NULL,
      casePostOutcomeDuration = NULL,
      includeTimeToEvent = NULL,
      includeDechallengeRechallenge = NULL,
      includeTargetBaseline = NULL,
      includeRiskFactors = NULL,
      includeCaseSeries = NULL,
      mode = "CohortIncidence",
      minSMD = 0,
      minCharacterizationMean = 0,
      minCovariateCount = 0,
      minTargetSize = 0,
      minCaseSize = 0,
      outputTable = "characterization_cohorts"
    )

#### Arguments

- `characterizationSettings`:

  The settings defined using
  Characterization::createCharacterizationSettings

- `targetIds`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `limitToFirstInNDays`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `minPriorObservation`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `outcomeIds`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `outcomeWashoutDays`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `riskWindowStart`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `startAnchor`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `riskWindowEnd`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `endAnchor`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `dechallengeStopInterval`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `dechallengeEvaluationWindow`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `covariateSettings`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `caseCovariateSettings`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `casePreTargetDuration`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `casePostOutcomeDuration`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `includeTimeToEvent`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `includeDechallengeRechallenge`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `includeTargetBaseline`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `includeRiskFactors`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `includeCaseSeries`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

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

- `minTargetSize`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `minCaseSize`:

  Deprecated. Use Characterization v4 settings objects via
  `characterizationSettings`.

- `outputTable`:

  The table used by characterization to create the cohorts used in
  characterization (included targets, cases, non-cases, etc.). The spec
  hash and database hash are added to this name to make it unique per
  run.

------------------------------------------------------------------------

### `CharacterizationModule$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CharacterizationModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
