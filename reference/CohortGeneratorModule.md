# Generate cohorts with the [HADES CohortGenerator Package](https://ohdsi.github.io/CohortGenerator/)

Generates cohorts against the OMOP Common Data Model

## Super class

[`Strategus::StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `CohortGeneratorModule`

## Public fields

- `cohortDefinitionSharedResourcesClassName`:

  A constant for the name of the cohort definition shared resources
  section of the analysis specification

- `negativeControlOutcomeSharedResourcesClassName`:

  A constant for the name of the negative control outcome shared
  resources section of the analysis specification

## Methods

### Public methods

- [`CohortGeneratorModule$new()`](#method-CohortGeneratorModule-new)

- [`CohortGeneratorModule$execute()`](#method-CohortGeneratorModule-execute)

- [`CohortGeneratorModule$createResultsDataModel()`](#method-CohortGeneratorModule-createResultsDataModel)

- [`CohortGeneratorModule$getResultsDataModelSpecification()`](#method-CohortGeneratorModule-getResultsDataModelSpecification)

- [`CohortGeneratorModule$uploadResults()`](#method-CohortGeneratorModule-uploadResults)

- [`CohortGeneratorModule$createModuleSpecifications()`](#method-CohortGeneratorModule-createModuleSpecifications)

- [`CohortGeneratorModule$createCohortSharedResourceSpecifications()`](#method-CohortGeneratorModule-createCohortSharedResourceSpecifications)

- [`CohortGeneratorModule$createNegativeControlOutcomeCohortSharedResourceSpecifications()`](#method-CohortGeneratorModule-createNegativeControlOutcomeCohortSharedResourceSpecifications)

- [`CohortGeneratorModule$validateModuleSpecifications()`](#method-CohortGeneratorModule-validateModuleSpecifications)

- [`CohortGeneratorModule$validateCohortSharedResourceSpecifications()`](#method-CohortGeneratorModule-validateCohortSharedResourceSpecifications)

- [`CohortGeneratorModule$validateNegativeControlOutcomeCohortSharedResourceSpecifications()`](#method-CohortGeneratorModule-validateNegativeControlOutcomeCohortSharedResourceSpecifications)

- [`CohortGeneratorModule$clone()`](#method-CohortGeneratorModule-clone)

Inherited methods

- [`Strategus::StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`Strategus::StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    CohortGeneratorModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Generates the cohorts

#### Usage

    CohortGeneratorModule$execute(
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

    CohortGeneratorModule$createResultsDataModel(
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

    CohortGeneratorModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for the module

#### Usage

    CohortGeneratorModule$uploadResults(
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

Creates the CohortGenerator Module Specifications

#### Usage

    CohortGeneratorModule$createModuleSpecifications(generateStats = TRUE)

#### Arguments

- `generateStats`:

  When TRUE, the Circe cohort definition SQL will include steps to
  compute inclusion rule statistics.

------------------------------------------------------------------------

### Method `createCohortSharedResourceSpecifications()`

Create shared specifications for the cohort definition set

#### Usage

    CohortGeneratorModule$createCohortSharedResourceSpecifications(
      cohortDefinitionSet
    )

#### Arguments

- `cohortDefinitionSet`:

  The cohort definition set to include in the specification. See the
  CohortGenerator package for details on how to build this object.

------------------------------------------------------------------------

### Method `createNegativeControlOutcomeCohortSharedResourceSpecifications()`

Create shared specifications for the negative control outcomes cohort
set

#### Usage

    CohortGeneratorModule$createNegativeControlOutcomeCohortSharedResourceSpecifications(
      negativeControlOutcomeCohortSet,
      occurrenceType,
      detectOnDescendants
    )

#### Arguments

- `negativeControlOutcomeCohortSet`:

  The negative control outcome cohort definition set defines the
  concepts to use to construct negative control outcome cohorts. See the
  CohortGenerator package for more details.

- `occurrenceType`:

  Either "first" or "all

- `detectOnDescendants`:

  When TRUE, the concept ID for the negative control will use the
  `concept_ancestor` table and will detect descendant concepts when
  constructing the cohort.

------------------------------------------------------------------------

### Method `validateModuleSpecifications()`

Validate the module specifications

#### Usage

    CohortGeneratorModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  The CohortGenerator module specifications

------------------------------------------------------------------------

### Method `validateCohortSharedResourceSpecifications()`

Validate the cohort shared resource specifications

#### Usage

    CohortGeneratorModule$validateCohortSharedResourceSpecifications(
      cohortSharedResourceSpecifications
    )

#### Arguments

- `cohortSharedResourceSpecifications`:

  The cohort shared resource specifications

------------------------------------------------------------------------

### Method `validateNegativeControlOutcomeCohortSharedResourceSpecifications()`

Validate the cohort shared resource specifications

#### Usage

    CohortGeneratorModule$validateNegativeControlOutcomeCohortSharedResourceSpecifications(
      negativeControlOutcomeCohortSharedResourceSpecifications
    )

#### Arguments

- `negativeControlOutcomeCohortSharedResourceSpecifications`:

  The cohort shared resource specifications

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortGeneratorModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
