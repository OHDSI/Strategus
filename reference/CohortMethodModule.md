# New-user cohort studies with the [HADES CohortMethod Package](https://ohdsi.github.io/CohortMethod/)

Module for performing new-user cohort studies against the OMOP Common
Data Model

## Super class

[`Strategus::StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `CohortMethodModule`

## Methods

### Public methods

- [`CohortMethodModule$new()`](#method-CohortMethodModule-new)

- [`CohortMethodModule$execute()`](#method-CohortMethodModule-execute)

- [`CohortMethodModule$createResultsDataModel()`](#method-CohortMethodModule-createResultsDataModel)

- [`CohortMethodModule$getResultsDataModelSpecification()`](#method-CohortMethodModule-getResultsDataModelSpecification)

- [`CohortMethodModule$uploadResults()`](#method-CohortMethodModule-uploadResults)

- [`CohortMethodModule$createModuleSpecifications()`](#method-CohortMethodModule-createModuleSpecifications)

- [`CohortMethodModule$validateModuleSpecifications()`](#method-CohortMethodModule-validateModuleSpecifications)

- [`CohortMethodModule$clone()`](#method-CohortMethodModule-clone)

Inherited methods

- [`Strategus::StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`Strategus::StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    CohortMethodModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Executes the CohortMethod package

#### Usage

    CohortMethodModule$execute(
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

  The analysis specifications for the study

- `executionSettings`:

  An object of type `ExecutionSettings` as created by
  [`createCdmExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createCdmExecutionSettings.md)
  or
  [`createResultsExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsExecutionSettings.md).

------------------------------------------------------------------------

### Method `createResultsDataModel()`

Create the results data model for the module

#### Usage

    CohortMethodModule$createResultsDataModel(
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

    CohortMethodModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for the module

#### Usage

    CohortMethodModule$uploadResults(
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

- `resultsDataModelSettings`:

  The results data model settings as created using \[@seealso
  [`createResultsDataModelSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsDataModelSettings.md)\]

------------------------------------------------------------------------

### Method `createModuleSpecifications()`

Creates the CohortMethod Module Specifications

#### Usage

    CohortMethodModule$createModuleSpecifications(
      cmAnalysesSpecifications,
      cmAnalysisList = NULL,
      targetComparatorOutcomesList = NULL,
      analysesToExclude = NULL,
      refitPsForEveryOutcome = NULL,
      refitPsForEveryStudyPopulation = NULL,
      cmDiagnosticThresholds = NULL
    )

#### Arguments

- `cmAnalysesSpecifications`:

  An R6 class created by CohortMethod::createCmAnalysesSpecifications

- `cmAnalysisList`:

  Deprecated with CohortMethod v6 - please use the
  `cmAnalysesSpecifications` parameter instead.

- `targetComparatorOutcomesList`:

  Deprecated with CohortMethod v6 - please use the
  `cmAnalysesSpecifications` parameter instead.

- `analysesToExclude`:

  Deprecated with CohortMethod v6 - please use the
  `cmAnalysesSpecifications` parameter instead.

- `refitPsForEveryOutcome`:

  Deprecated with CohortMethod v6 - please use the
  `cmAnalysesSpecifications` parameter instead.

- `refitPsForEveryStudyPopulation`:

  Deprecated with CohortMethod v6 - please use the
  `cmAnalysesSpecifications` parameter instead.

- `cmDiagnosticThresholds`:

  Deprecated with CohortMethod v6 - please use the
  `cmAnalysesSpecifications` parameter instead.

#### Details

Run a list of analyses for the target-comparator-outcomes of interest.
This function will run all specified analyses against all hypotheses of
interest, meaning that the total number of outcome models is
`length(cmAnalysisList) * length(targetComparatorOutcomesList)` (if all
analyses specify an outcome model should be fitted). When you provide
several analyses it will determine whether any of the analyses have
anything in common, and will take advantage of this fact. For example,
if we specify several analyses that only differ in the way the outcome
model is fitted, then this function will extract the data and fit the
propensity model only once, and re-use this in all the analysis.

After completion, a tibble containing references to all generated files
can be obtained using the
[`CohortMethod::getFileReference()`](https://ohdsi.github.io/CohortMethod/reference/getFileReference.html)
function. A summary of the analysis results can be obtained using the
[`CohortMethod::getResultsSummary()`](https://ohdsi.github.io/CohortMethod/reference/getResultsSummary.html)
function.

##### Analyses to Exclude

Normally, `runCmAnalyses` will run all combinations of
target-comparator-outcome-analyses settings. However, sometimes we may
not need all those combinations. Using the `analysesToExclude` argument,
we can remove certain items from the full matrix. This argument should
be a data frame with at least one of the following columns:

------------------------------------------------------------------------

### Method `validateModuleSpecifications()`

Validate the module specifications

#### Usage

    CohortMethodModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  The CohortMethod module specifications

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CohortMethodModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
