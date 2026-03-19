# Validation of patient-level prediction models with the [HADES PatientLevelPrediction Package](https://ohdsi.github.io/PatientLevelPrediction/)

Module for performing patient-level prediction model validation for
models built using the PatientLevelPrediction package.

## Super class

[`Strategus::StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `PatientLevelPredictionValidationModule`

## Public fields

- `tablePrefix`:

  The table prefix to append to the results tables

## Methods

### Public methods

- [`PatientLevelPredictionValidationModule$new()`](#method-PatientLevelPredictionValidationModule-new)

- [`PatientLevelPredictionValidationModule$execute()`](#method-PatientLevelPredictionValidationModule-execute)

- [`PatientLevelPredictionValidationModule$createResultsDataModel()`](#method-PatientLevelPredictionValidationModule-createResultsDataModel)

- [`PatientLevelPredictionValidationModule$uploadResults()`](#method-PatientLevelPredictionValidationModule-uploadResults)

- [`PatientLevelPredictionValidationModule$createModuleSpecifications()`](#method-PatientLevelPredictionValidationModule-createModuleSpecifications)

- [`PatientLevelPredictionValidationModule$validateModuleSpecifications()`](#method-PatientLevelPredictionValidationModule-validateModuleSpecifications)

- [`PatientLevelPredictionValidationModule$clone()`](#method-PatientLevelPredictionValidationModule-clone)

Inherited methods

- [`Strategus::StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`Strategus::StrategusModule$getResultsDataModelSpecification()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-getResultsDataModelSpecification)
- [`Strategus::StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    PatientLevelPredictionValidationModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Executes the PatientLevelPrediction package to validate a PLP model

#### Usage

    PatientLevelPredictionValidationModule$execute(
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

    PatientLevelPredictionValidationModule$createResultsDataModel(
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

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for the module

#### Usage

    PatientLevelPredictionValidationModule$uploadResults(
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

Creates the PatientLevelPredictionValidation Module Specifications

#### Usage

    PatientLevelPredictionValidationModule$createModuleSpecifications(
      validationList = list(PatientLevelPrediction::createValidationDesign(plpModelList =
        list(file.path("location_to_model")), targetId = 1, outcomeId = 3,
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
        populationSettings = NULL, recalibrate = "weakRecalibration", runCovariateSummary =
        TRUE), PatientLevelPrediction::createValidationDesign(plpModelList =
        list(file.path("location_to_model")), targetId = 4, outcomeId = 3,
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),

           populationSettings = NULL, recalibrate = "weakRecalibration", runCovariateSummary
        = TRUE)),
      logLevel = "INFO"
    )

#### Arguments

- `validationList`:

  A list of validation designs from
  [`PatientLevelPrediction::createValidationDesign`](https://ohdsi.github.io/PatientLevelPrediction/reference/createValidationDesign.html)

- `logLevel`:

  The logging level while executing the model validation.

------------------------------------------------------------------------

### Method `validateModuleSpecifications()`

Validate the module specifications

#### Usage

    PatientLevelPredictionValidationModule$validateModuleSpecifications(
      moduleSpecifications
    )

#### Arguments

- `moduleSpecifications`:

  The PatientLevelPredictionValidation module specifications

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PatientLevelPredictionValidationModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
