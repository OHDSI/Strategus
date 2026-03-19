# Patient-level prediction with the [HADES PatientLevelPrediction Package](https://ohdsi.github.io/PatientLevelPrediction/)

Module for performing patient-level prediction in an observational
database in the OMOP Common Data Model.

## Super class

[`Strategus::StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `PatientLevelPredictionModule`

## Public fields

- `tablePrefix`:

  The table prefix to append to the results tables

## Methods

### Public methods

- [`PatientLevelPredictionModule$new()`](#method-PatientLevelPredictionModule-new)

- [`PatientLevelPredictionModule$execute()`](#method-PatientLevelPredictionModule-execute)

- [`PatientLevelPredictionModule$createResultsDataModel()`](#method-PatientLevelPredictionModule-createResultsDataModel)

- [`PatientLevelPredictionModule$getResultsDataModelSpecification()`](#method-PatientLevelPredictionModule-getResultsDataModelSpecification)

- [`PatientLevelPredictionModule$uploadResults()`](#method-PatientLevelPredictionModule-uploadResults)

- [`PatientLevelPredictionModule$createModuleSpecifications()`](#method-PatientLevelPredictionModule-createModuleSpecifications)

- [`PatientLevelPredictionModule$validateModuleSpecifications()`](#method-PatientLevelPredictionModule-validateModuleSpecifications)

- [`PatientLevelPredictionModule$clone()`](#method-PatientLevelPredictionModule-clone)

Inherited methods

- [`Strategus::StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`Strategus::StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    PatientLevelPredictionModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Executes the PatientLevelPrediction package

#### Usage

    PatientLevelPredictionModule$execute(
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

    PatientLevelPredictionModule$createResultsDataModel(
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

    PatientLevelPredictionModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for the module

#### Usage

    PatientLevelPredictionModule$uploadResults(
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

Creates the PatientLevelPrediction Module Specifications

#### Usage

    PatientLevelPredictionModule$createModuleSpecifications(
      modelDesignList,
      skipDiagnostics = FALSE
    )

#### Arguments

- `modelDesignList`:

  A list of model designs created using
  [`PatientLevelPrediction::createModelDesign()`](https://ohdsi.github.io/PatientLevelPrediction/reference/createModelDesign.html)

- `skipDiagnostics`:

  Whether to run the diagnostics based on PROBAST before model
  development

------------------------------------------------------------------------

### Method `validateModuleSpecifications()`

Validate the module specifications

#### Usage

    PatientLevelPredictionModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  The PatientLevelPrediction module specifications

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PatientLevelPredictionModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
