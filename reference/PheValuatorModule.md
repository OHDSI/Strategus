# Evaluate phenotype algorithms with the [HADES PheValuator Package](https://ohdsi.github.io/PheValuator/)

Evaluates phenotype algorithms using the PheValuator method against the
OMOP Common Data Model.

## Super class

[`StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `PheValuatorModule`

## Public fields

- `tablePrefix`:

  The table prefix to append to the results tables

## Methods

### Public methods

- [`PheValuatorModule$new()`](#method-PheValuatorModule-initialize)

- [`PheValuatorModule$execute()`](#method-PheValuatorModule-execute)

- [`PheValuatorModule$createResultsDataModel()`](#method-PheValuatorModule-createResultsDataModel)

- [`PheValuatorModule$getResultsDataModelSpecification()`](#method-PheValuatorModule-getResultsDataModelSpecification)

- [`PheValuatorModule$uploadResults()`](#method-PheValuatorModule-uploadResults)

- [`PheValuatorModule$createModuleSpecifications()`](#method-PheValuatorModule-createModuleSpecifications)

- [`PheValuatorModule$validateModuleSpecifications()`](#method-PheValuatorModule-validateModuleSpecifications)

- [`PheValuatorModule$clone()`](#method-PheValuatorModule-clone)

Inherited methods

- [`StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`StrategusModule$getPackageInformation()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-getPackageInformation)
- [`StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### `PheValuatorModule$new()`

Initialize the module

#### Usage

    PheValuatorModule$new()

------------------------------------------------------------------------

### `PheValuatorModule$execute()`

Execute PheValuator

#### Usage

    PheValuatorModule$execute(
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

### `PheValuatorModule$createResultsDataModel()`

Create the results data model for the module

#### Usage

    PheValuatorModule$createResultsDataModel(
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

### `PheValuatorModule$getResultsDataModelSpecification()`

Get the results data model specification for the module

#### Usage

    PheValuatorModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### `PheValuatorModule$uploadResults()`

Upload the results for PheValuator

#### Usage

    PheValuatorModule$uploadResults(
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

### `PheValuatorModule$createModuleSpecifications()`

Creates the PheValuator Module Specifications

#### Usage

    PheValuatorModule$createModuleSpecifications(
      analysisName = "Main",
      pheValuatorAnalysisList
    )

#### Arguments

- `analysisName`:

  A short name for the analysis (default: `"Main"`).

- `pheValuatorAnalysisList`:

  A list of analysis specification objects. Each element is a list with
  two named fields:

  `phenotype`

  :   A short file-system-safe name for the phenotype (e.g.
      `"bladderCancer"`).

  `cohortsToEvaluate`

  :   A list of evaluation parameters:

      `phenotypeCohortId`

      :   Cohort ID(s) of the phenotype to evaluate.

      `washoutPeriod`

      :   Minimum prior observation days (should match cohort
          definition).

      `xSpecCohortId`

      :   Cohort ID for the extremely-specific (xSpec) cohort.

      `daysFromxSpec`

      :   Days from xSpec cohort start to index visit.

      `xSensCohortId`

      :   Cohort ID for the extremely-sensitive (xSens) cohort.

      `prevalenceCohortId`

      :   Cohort ID used to estimate prevalence.

      `excludedCovariateConceptIds`

      :   Integer vector of concept IDs to exclude from covariates.

      `covariateSettingsType`

      :   One of `"chronic"` or `"acute"` (default `"chronic"`).

------------------------------------------------------------------------

### `PheValuatorModule$validateModuleSpecifications()`

Validate the module specifications

#### Usage

    PheValuatorModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  The PheValuator module specifications

------------------------------------------------------------------------

### `PheValuatorModule$clone()`

The objects of this class are cloneable with this method.

#### Usage

    PheValuatorModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
