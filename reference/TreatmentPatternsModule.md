# Evaluate phenotypes with the [DARWIN TreatmentPatterns Package](https://github.com/darwin-eu/TreatmentPatterns/)

Characterization and description of patterns of events (cohorts).
against the OMOP Common Data Model.

## Super class

[`Strategus::StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `TreatmentPatternsModule`

## Public fields

- `tablePrefix`:

  The table prefix to append to the results tables

## Methods

### Public methods

- [`TreatmentPatternsModule$new()`](#method-TreatmentPatternsModule-new)

- [`TreatmentPatternsModule$execute()`](#method-TreatmentPatternsModule-execute)

- [`TreatmentPatternsModule$createResultsDataModel()`](#method-TreatmentPatternsModule-createResultsDataModel)

- [`TreatmentPatternsModule$getResultsDataModelSpecification()`](#method-TreatmentPatternsModule-getResultsDataModelSpecification)

- [`TreatmentPatternsModule$uploadResults()`](#method-TreatmentPatternsModule-uploadResults)

- [`TreatmentPatternsModule$createModuleSpecifications()`](#method-TreatmentPatternsModule-createModuleSpecifications)

- [`TreatmentPatternsModule$createMultiAnalysisModuleSpecification()`](#method-TreatmentPatternsModule-createMultiAnalysisModuleSpecification)

- [`TreatmentPatternsModule$validateModuleSpecifications()`](#method-TreatmentPatternsModule-validateModuleSpecifications)

- [`TreatmentPatternsModule$clone()`](#method-TreatmentPatternsModule-clone)

Inherited methods

- [`Strategus::StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`Strategus::StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    TreatmentPatternsModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Execute Treatment Patterns

#### Usage

    TreatmentPatternsModule$execute(
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

    TreatmentPatternsModule$createResultsDataModel(
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

    TreatmentPatternsModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for TreatmentPatterns

#### Usage

    TreatmentPatternsModule$uploadResults(
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

Creates the TreatmentPatternsnModule Specifications

#### Usage

    TreatmentPatternsModule$createModuleSpecifications(
      analysisId = 1,
      cohorts,
      description = "",
      includeTreatments = NULL,
      indexDateOffset = NULL,
      minEraDuration = 0,
      splitEventCohorts = NULL,
      splitTime = NULL,
      eraCollapseSize = 30,
      combinationWindow = 30,
      minPostCombinationDuration = 30,
      filterTreatments = "First",
      maxPathLength = 5,
      ageWindow = 5,
      minCellCount = 1,
      censorType = "minCellCount",
      overlapMethod = "truncate",
      concatTargets = TRUE,
      startAnchor = "startDate",
      windowStart = 0,
      endAnchor = "endDate",
      windowEnd = 0,
      stratify = FALSE
    )

#### Arguments

- `analysisId`:

  (`numeric(1)`) Unique identifier for the TreatmentPatterns analysis

- `cohorts`:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  Data frame containing the following columns and data types:

  cohortId `numeric(1)`

  :   Cohort ID's of the cohorts to be used in the cohort table.

  cohortName `character(1)`

  :   Cohort names of the cohorts to be used in the cohort table.

  type `character(1)` \["target", "event', "exit"\]

  :   Cohort type, describing if the cohort is a target, event, or exit
      cohort

- `description`:

  (`character(1)`) Description for analysis

- `includeTreatments`:

  (`character(1)`: `"startDate"`)  
  `DEPRECATED`

  `"startDate"`

  :   Include treatments after the target cohort start date and onwards.

  `"endDate"`

  :   Include treatments before target cohort end date and before.

- `indexDateOffset`:

  (`integer(1)`: `0`)  
  `DEPRECATED` Offset the index date of the `Target` cohort.

- `minEraDuration`:

  (`integer(1)`: `0`)  
  Minimum time an event era should last to be included in analysis

- `splitEventCohorts`:

  (`character(n)`: `""`)  
  Specify event cohort to split in acute (\< X days) and therapy (\>= X
  days)

- `splitTime`:

  (`integer(1)`: `30`)  
  Specify number of days (X) at which each of the split event cohorts
  should be split in acute and therapy

- `eraCollapseSize`:

  (`integer(1)`: `30`)  
  Window of time between which two eras of the same event cohort are
  collapsed into one era

- `combinationWindow`:

  (`integer(1)`: `30`)  
  Window of time two event cohorts need to overlap to be considered a
  combination treatment

- `minPostCombinationDuration`:

  (`integer(1)`: `30`)  
  Minimum time an event era before or after a generated combination
  treatment should last to be included in analysis

- `filterTreatments`:

  (`character(1)`: `"First"` \["first", "Changes", "all"\])  
  Select first occurrence of (‘First’); changes between (‘Changes’); or
  all event cohorts (‘All’).

- `maxPathLength`:

  (`integer(1)`: `5`)  
  Maximum number of steps included in treatment pathway

- `ageWindow`:

  (`integer(n)`: `10`)  
  Number of years to bin age groups into. It may also be a vector of
  integers. I.e. `c(0, 18, 150)` which will results in age group `0-18`
  which includes subjects `< 19`. And age group `18-150` which includes
  subjects `> 18`.

- `minCellCount`:

  (`integer(1)`: `5`)  
  Minimum count required per pathway. Censors data below `x` as `<x`.
  This minimum value will carry over to the sankey diagram and sunburst
  plot.

- `censorType`:

  (`character(1)`)  

  `"minCellCount"`

  :   Censors pathways \<`minCellCount` to `minCellCount`.

  `"remove"`

  :   Censors pathways \<`minCellCount` by removing them completely.

  `"mean"`

  :   Censors pathways \<`minCellCount` to the mean of all frequencies
      below `minCellCount`

- `overlapMethod`:

  (`character(1)`: `"truncate"`) Method to decide how to deal with
  overlap that is not significant enough for combination. `"keep"` will
  keep the dates as is. `"truncate"` truncates the first occurring event
  to the start date of the next event.

- `concatTargets`:

  (`logical(1)`: `TRUE`) Should multiple target cohorts for the same
  person be concatenated or not?

- `startAnchor`:

  (`character(1)`: `"startDate"`) Start date anchor. One of:
  `"startDate"`, `"endDate"`

- `windowStart`:

  (`numeric(1)`: `0`) Offset for `startAnchor` in days.

- `endAnchor`:

  (`character(1)`: `"endDate"`) End date anchor. One of: `"startDate"`,
  `"endDate"`

- `windowEnd`:

  (`numeric(1)`: `0`) Offset for `endAnchor` in days.

- `stratify`:

  (`logical(1)`)  
  This will perform pairwise stratification between age, sex, and index
  year if set TRUE

------------------------------------------------------------------------

### Method `createMultiAnalysisModuleSpecification()`

Runs multiple analyses using a list of analysis specification objects
(as produced by`createModuleSpecifications`) into a single module
specification

#### Usage

    TreatmentPatternsModule$createMultiAnalysisModuleSpecification(tpAnalysisList)

#### Arguments

- `tpAnalysisList`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  A list of analysis specification objects. Each element should be a
  list created by `createAnalysisSpecification`

------------------------------------------------------------------------

### Method `validateModuleSpecifications()`

Validate the module specifications

#### Usage

    TreatmentPatternsModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  The treatment patterns module specifications

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TreatmentPatternsModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
