# Compute incidence with the [HADES CohortIncidence Package](https://ohdsi.github.io/CohortIncidence/)

Computes incidence rates for cohorts against the OMOP Common Data Model

## Super class

[`StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `CohortIncidenceModule`

## Public fields

- `tablePrefix`:

  The table prefix to append to results tables

## Methods

### Public methods

- [`CohortIncidenceModule$new()`](#method-CohortIncidenceModule-initialize)

- [`CohortIncidenceModule$execute()`](#method-CohortIncidenceModule-execute)

- [`CohortIncidenceModule$createResultsDataModel()`](#method-CohortIncidenceModule-createResultsDataModel)

- [`CohortIncidenceModule$getResultsDataModelSpecification()`](#method-CohortIncidenceModule-getResultsDataModelSpecification)

- [`CohortIncidenceModule$uploadResults()`](#method-CohortIncidenceModule-uploadResults)

- [`CohortIncidenceModule$createModuleSpecifications()`](#method-CohortIncidenceModule-createModuleSpecifications)

- [`CohortIncidenceModule$validateModuleSpecifications()`](#method-CohortIncidenceModule-validateModuleSpecifications)

- [`CohortIncidenceModule$clone()`](#method-CohortIncidenceModule-clone)

Inherited methods

- [`StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`StrategusModule$getPackageInformation()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-getPackageInformation)
- [`StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### `CohortIncidenceModule$new()`

Initialize the module

#### Usage

    CohortIncidenceModule$new()

------------------------------------------------------------------------

### `CohortIncidenceModule$execute()`

Execute the CohortIncidence package

#### Usage

    CohortIncidenceModule$execute(
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

### `CohortIncidenceModule$createResultsDataModel()`

Create the results data model for the module

#### Usage

    CohortIncidenceModule$createResultsDataModel(
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

### `CohortIncidenceModule$getResultsDataModelSpecification()`

Get the results data model specification for the module

#### Usage

    CohortIncidenceModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### `CohortIncidenceModule$uploadResults()`

Upload the results for the module

#### Usage

    CohortIncidenceModule$uploadResults(
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

### `CohortIncidenceModule$createModuleSpecifications()`

Creates the CohortIncidence Module Specifications

#### Usage

    CohortIncidenceModule$createModuleSpecifications(irDesign = NULL)

#### Arguments

- `irDesign`:

  The incidence rate design created from the CohortIncidence package

------------------------------------------------------------------------

### `CohortIncidenceModule$validateModuleSpecifications()`

Validate the module specifications

#### Usage

    CohortIncidenceModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  The CohortIncidence module specifications

------------------------------------------------------------------------

### `CohortIncidenceModule$clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortIncidenceModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
