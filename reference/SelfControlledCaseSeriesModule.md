# Self-Controlled Case Series design with the [HADES SelfControlledCaseSeries Package](https://ohdsi.github.io/SelfControlledCaseSeries/)

Module for performing Self-Controlled Case Series (SCCS) analyses
against the OMOP Common Data Model.

## Super class

[`StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `SelfControlledCaseSeriesModule`

## Public fields

- `tablePrefix`:

  The table prefix for results tables

## Methods

### Public methods

- [`SelfControlledCaseSeriesModule$new()`](#method-SelfControlledCaseSeriesModule-initialize)

- [`SelfControlledCaseSeriesModule$execute()`](#method-SelfControlledCaseSeriesModule-execute)

- [`SelfControlledCaseSeriesModule$createResultsDataModel()`](#method-SelfControlledCaseSeriesModule-createResultsDataModel)

- [`SelfControlledCaseSeriesModule$getResultsDataModelSpecification()`](#method-SelfControlledCaseSeriesModule-getResultsDataModelSpecification)

- [`SelfControlledCaseSeriesModule$uploadResults()`](#method-SelfControlledCaseSeriesModule-uploadResults)

- [`SelfControlledCaseSeriesModule$createModuleSpecifications()`](#method-SelfControlledCaseSeriesModule-createModuleSpecifications)

- [`SelfControlledCaseSeriesModule$validateModuleSpecifications()`](#method-SelfControlledCaseSeriesModule-validateModuleSpecifications)

- [`SelfControlledCaseSeriesModule$clone()`](#method-SelfControlledCaseSeriesModule-clone)

Inherited methods

- [`StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`StrategusModule$getPackageInformation()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-getPackageInformation)
- [`StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### `SelfControlledCaseSeriesModule$new()`

Initialize the module

#### Usage

    SelfControlledCaseSeriesModule$new()

------------------------------------------------------------------------

### `SelfControlledCaseSeriesModule$execute()`

Executes the SelfControlledCaseSeries package

#### Usage

    SelfControlledCaseSeriesModule$execute(
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

### `SelfControlledCaseSeriesModule$createResultsDataModel()`

Create the results data model for the module

#### Usage

    SelfControlledCaseSeriesModule$createResultsDataModel(
      resultsConnectionDetails,
      resultsDatabaseSchema,
      tablePrefix = ""
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

### `SelfControlledCaseSeriesModule$getResultsDataModelSpecification()`

Get the results data model specification for the module

#### Usage

    SelfControlledCaseSeriesModule$getResultsDataModelSpecification(
      tablePrefix = ""
    )

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### `SelfControlledCaseSeriesModule$uploadResults()`

Upload the results for the module

#### Usage

    SelfControlledCaseSeriesModule$uploadResults(
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

### `SelfControlledCaseSeriesModule$createModuleSpecifications()`

Creates the SelfControlledCaseSeries Module Specifications

#### Usage

    SelfControlledCaseSeriesModule$createModuleSpecifications(
      sccsAnalysesSpecifications,
      sccsAnalysisList = NULL,
      exposuresOutcomeList = NULL,
      analysesToExclude = NULL,
      combineDataFetchAcrossOutcomes = NULL,
      sccsDiagnosticThresholds = NULL
    )

#### Arguments

- `sccsAnalysesSpecifications`:

  An R6 class created by
  SelfControlledCaseSeries::createSccsAnalysesSpecifications

- `sccsAnalysisList`:

  Deprecated with SelfControlledCaseSeries v6 - please use the
  `sccsAnalysesSpecifications` parameter instead.

- `exposuresOutcomeList`:

  Deprecated with SelfControlledCaseSeries v6 - please use the
  `sccsAnalysesSpecifications` parameter instead.

- `analysesToExclude`:

  Deprecated with SelfControlledCaseSeries v6 - please use the
  `sccsAnalysesSpecifications` parameter instead.

- `combineDataFetchAcrossOutcomes`:

  Deprecated with SelfControlledCaseSeries v6 - please use the
  `sccsAnalysesSpecifications` parameter instead.

- `sccsDiagnosticThresholds`:

  Deprecated with SelfControlledCaseSeries v6 - please use the
  `sccsAnalysesSpecifications` parameter instead.

------------------------------------------------------------------------

### `SelfControlledCaseSeriesModule$validateModuleSpecifications()`

Validate the module specifications

#### Usage

    SelfControlledCaseSeriesModule$validateModuleSpecifications(
      moduleSpecifications
    )

#### Arguments

- `moduleSpecifications`:

  The SelfControlledCaseSeries module specifications

------------------------------------------------------------------------

### `SelfControlledCaseSeriesModule$clone()`

The objects of this class are cloneable with this method.

#### Usage

    SelfControlledCaseSeriesModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
