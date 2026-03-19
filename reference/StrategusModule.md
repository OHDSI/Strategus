# StrategusModule defines the base class for each HADES module

StrategusModule serves as an internal base class that defines the core
functions and structure to be inherited and implemented by any specific
HADES module. It provides a standardized framework for creating modular
components within the Strategus pipeline.

## Public fields

- `moduleName`:

  The name of the module taken from the class name. This is set in the
  constructor of the class.

- `moduleClassName`:

  The class name that identifies the module specifications in the
  overall analysis specification. This is set in the constructor of the
  class.

- `internalModuleSpecificationClassName`:

  A constant value. The base class name that identifies a module
  specification in the analysis specification.

- `internalSharedResourcesClassName`:

  A constant value. The class name that identifies the shared resources
  section in the overall analysis specification.

## Methods

### Public methods

- [`StrategusModule$new()`](#method-StrategusModule-new)

- [`StrategusModule$execute()`](#method-StrategusModule-execute)

- [`StrategusModule$createResultsDataModel()`](#method-StrategusModule-createResultsDataModel)

- [`StrategusModule$getResultsDataModelSpecification()`](#method-StrategusModule-getResultsDataModelSpecification)

- [`StrategusModule$uploadResults()`](#method-StrategusModule-uploadResults)

- [`StrategusModule$createModuleSpecifications()`](#method-StrategusModule-createModuleSpecifications)

- [`StrategusModule$createSharedResourcesSpecifications()`](#method-StrategusModule-createSharedResourcesSpecifications)

- [`StrategusModule$validateModuleSpecifications()`](#method-StrategusModule-validateModuleSpecifications)

- [`StrategusModule$validateSharedResourcesSpecifications()`](#method-StrategusModule-validateSharedResourcesSpecifications)

- [`StrategusModule$clone()`](#method-StrategusModule-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    StrategusModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Executes the module

#### Usage

    StrategusModule$execute(
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

    StrategusModule$createResultsDataModel(
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

### Method `getResultsDataModelSpecification()`

Get the results data model specification for the module

#### Usage

    StrategusModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for the module

#### Usage

    StrategusModule$uploadResults(
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

Base function for creating the module settings object. Each module will
have its own implementation and this base class method will be used to
ensure the class of the specifications is set properly.

#### Usage

    StrategusModule$createModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  An object of type `ModuleSpecifications`

- `moduleSpecifications`:

  An object of type `ModuleSpecifications`

------------------------------------------------------------------------

### Method `createSharedResourcesSpecifications()`

Base function for creating the shared resources settings object. Each
module will have its own implementation if it needs to create a shared
resource.

#### Usage

    StrategusModule$createSharedResourcesSpecifications(
      className,
      sharedResourcesSpecifications
    )

#### Arguments

- `className`:

  The class name of the shared resources specifications

- `sharedResourcesSpecifications`:

  The shared resources specifications

------------------------------------------------------------------------

### Method `validateModuleSpecifications()`

Base function for validating the module settings object. Each module
will have its own implementation and this base class method will be used
to ensure the module specifications are valid ahead of execution

#### Usage

    StrategusModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  An object of type `ModuleSpecifications`

- `moduleSpecifications`:

  An object of type `ModuleSpecifications`

------------------------------------------------------------------------

### Method `validateSharedResourcesSpecifications()`

Base function for validating the shared resources specification settings
object. Each module will have its own implementation and this base class
method will be used to ensure the module specifications are valid ahead
of execution

#### Usage

    StrategusModule$validateSharedResourcesSpecifications(
      className,
      sharedResourcesSpecifications
    )

#### Arguments

- `className`:

  The class name of the shared resources specifications

- `sharedResourcesSpecifications`:

  The shared resources specifications

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    StrategusModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
