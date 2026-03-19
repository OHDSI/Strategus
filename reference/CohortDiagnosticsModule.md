# Evaluate phenotypes with the [HADES CohortDiagnostics Package](https://ohdsi.github.io/CohortDiagnostics/)

Development and evaluation of phenotype algorithms against the OMOP
Common Data Model.

## Super class

[`Strategus::StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `CohortDiagnosticsModule`

## Public fields

- `tablePrefix`:

  The table prefix to append to results tables

## Methods

### Public methods

- [`CohortDiagnosticsModule$new()`](#method-CohortDiagnosticsModule-new)

- [`CohortDiagnosticsModule$execute()`](#method-CohortDiagnosticsModule-execute)

- [`CohortDiagnosticsModule$createResultsDataModel()`](#method-CohortDiagnosticsModule-createResultsDataModel)

- [`CohortDiagnosticsModule$getResultsDataModelSpecification()`](#method-CohortDiagnosticsModule-getResultsDataModelSpecification)

- [`CohortDiagnosticsModule$uploadResults()`](#method-CohortDiagnosticsModule-uploadResults)

- [`CohortDiagnosticsModule$createModuleSpecifications()`](#method-CohortDiagnosticsModule-createModuleSpecifications)

- [`CohortDiagnosticsModule$validateModuleSpecifications()`](#method-CohortDiagnosticsModule-validateModuleSpecifications)

- [`CohortDiagnosticsModule$clone()`](#method-CohortDiagnosticsModule-clone)

Inherited methods

- [`Strategus::StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`Strategus::StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    CohortDiagnosticsModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Executes the CohortDiagnostics package

#### Usage

    CohortDiagnosticsModule$execute(
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

    CohortDiagnosticsModule$createResultsDataModel(
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

    CohortDiagnosticsModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for the module

#### Usage

    CohortDiagnosticsModule$uploadResults(
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

Creates the CohortDiagnostics Module Specifications

#### Usage

    CohortDiagnosticsModule$createModuleSpecifications(
      cohortIds = NULL,
      runInclusionStatistics = TRUE,
      runIncludedSourceConcepts = TRUE,
      runOrphanConcepts = TRUE,
      runTimeSeries = FALSE,
      runVisitContext = TRUE,
      runBreakdownIndexEvents = TRUE,
      runIncidenceRate = TRUE,
      runCohortRelationship = TRUE,
      runTemporalCohortCharacterization = TRUE,
      temporalCovariateSettings = private$.getDefaultCovariateSettings(),
      minCharacterizationMean = 0.01,
      irWashoutPeriod = 0
    )

#### Arguments

- `cohortIds`:

  A list of cohort IDs to use when running the CohortDiagnostics.
  Default is NULL which will use all cohorts present in the cohort
  definition set in the analysis specification

- `runInclusionStatistics`:

  Generate and export statistic on the cohort inclusion rules?

- `runIncludedSourceConcepts`:

  Generate and export the source concepts included in the cohorts?

- `runOrphanConcepts`:

  Generate and export potential orphan concepts?

- `runTimeSeries`:

  Generate and export the time series diagnostics?

- `runVisitContext`:

  Generate and export index-date visit context?

- `runBreakdownIndexEvents`:

  Generate and export the breakdown of index events?

- `runIncidenceRate`:

  Generate and export the cohort incidence rates?

- `runCohortRelationship`:

  Generate and export the cohort relationship? Cohort relationship
  checks the temporal relationship between two or more cohorts.

- `runTemporalCohortCharacterization`:

  Generate and export the temporal cohort characterization? Only records
  with values greater than 0.001 are returned.

- `temporalCovariateSettings`:

  Either an object of type `covariateSettings` as created using one of
  the createTemporalCovariateSettings function in the FeatureExtraction
  package, or a list of such objects.

- `minCharacterizationMean`:

  The minimum mean value for characterization output. Values below this
  will be cut off from output. This will help reduce the file size of
  the characterization output, but will remove information on covariates
  that have very low values. The default is 0.001 (i.e. 0.1 percent)

- `irWashoutPeriod`:

  Number of days washout to include in calculation of incidence rates -
  default is 0

------------------------------------------------------------------------

### Method `validateModuleSpecifications()`

Validate the module specifications

#### Usage

    CohortDiagnosticsModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  The CohortIncidence module specifications

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortDiagnosticsModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
