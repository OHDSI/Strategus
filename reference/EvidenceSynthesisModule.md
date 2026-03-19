# Meta-analysis with the [HADES EvidenceSynthesis Package](https://ohdsi.github.io/EvidenceSynthesis/)

Module for for combining causal effect estimates and study diagnostics
across multiple data sites in a distributed study. This includes
functions for performing meta-analysis and forest plots

## Super class

[`Strategus::StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
-\> `EvidenceSynthesisModule`

## Methods

### Public methods

- [`EvidenceSynthesisModule$new()`](#method-EvidenceSynthesisModule-new)

- [`EvidenceSynthesisModule$execute()`](#method-EvidenceSynthesisModule-execute)

- [`EvidenceSynthesisModule$createResultsDataModel()`](#method-EvidenceSynthesisModule-createResultsDataModel)

- [`EvidenceSynthesisModule$getResultsDataModelSpecification()`](#method-EvidenceSynthesisModule-getResultsDataModelSpecification)

- [`EvidenceSynthesisModule$uploadResults()`](#method-EvidenceSynthesisModule-uploadResults)

- [`EvidenceSynthesisModule$validateModuleSpecifications()`](#method-EvidenceSynthesisModule-validateModuleSpecifications)

- [`EvidenceSynthesisModule$createEvidenceSynthesisSource()`](#method-EvidenceSynthesisModule-createEvidenceSynthesisSource)

- [`EvidenceSynthesisModule$createRandomEffectsMetaAnalysis()`](#method-EvidenceSynthesisModule-createRandomEffectsMetaAnalysis)

- [`EvidenceSynthesisModule$createFixedEffectsMetaAnalysis()`](#method-EvidenceSynthesisModule-createFixedEffectsMetaAnalysis)

- [`EvidenceSynthesisModule$createBayesianMetaAnalysis()`](#method-EvidenceSynthesisModule-createBayesianMetaAnalysis)

- [`EvidenceSynthesisModule$createEsDiagnosticThresholds()`](#method-EvidenceSynthesisModule-createEsDiagnosticThresholds)

- [`EvidenceSynthesisModule$createModuleSpecifications()`](#method-EvidenceSynthesisModule-createModuleSpecifications)

- [`EvidenceSynthesisModule$clone()`](#method-EvidenceSynthesisModule-clone)

Inherited methods

- [`Strategus::StrategusModule$createSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-createSharedResourcesSpecifications)
- [`Strategus::StrategusModule$validateSharedResourcesSpecifications()`](https://ohdsi.github.io/Strategus/reference/StrategusModule.html#method-validateSharedResourcesSpecifications)

------------------------------------------------------------------------

### Method `new()`

Initialize the module

#### Usage

    EvidenceSynthesisModule$new()

------------------------------------------------------------------------

### Method [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)

Executes the EvidenceSynthesis package

#### Usage

    EvidenceSynthesisModule$execute(
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

    EvidenceSynthesisModule$createResultsDataModel(
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

    EvidenceSynthesisModule$getResultsDataModelSpecification(tablePrefix = "")

#### Arguments

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

- `tablePrefix`:

  A prefix to apply to the database table names (optional).

------------------------------------------------------------------------

### Method [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)

Upload the results for the module

#### Usage

    EvidenceSynthesisModule$uploadResults(
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

### Method `validateModuleSpecifications()`

Validate the module specifications

#### Usage

    EvidenceSynthesisModule$validateModuleSpecifications(moduleSpecifications)

#### Arguments

- `moduleSpecifications`:

  The EvidenceSynthesis module specifications Create an evidence
  synthesis source

------------------------------------------------------------------------

### Method `createEvidenceSynthesisSource()`

#### Usage

    EvidenceSynthesisModule$createEvidenceSynthesisSource(
      sourceMethod = "CohortMethod",
      databaseIds = NULL,
      analysisIds = NULL,
      likelihoodApproximation = "grid with gradients"
    )

#### Arguments

- `sourceMethod`:

  The source method generating the estimates to synthesize. Can be
  "CohortMethod" or "SelfControlledCaseSeries"

- `databaseIds`:

  The database IDs to include. Use `databaseIds = NULL` to include all
  database IDs.

- `analysisIds`:

  The source method analysis IDs to include. Use `analysisIds = NULL` to
  include all analysis IDs.

- `likelihoodApproximation`:

  The type of likelihood approximation. Can be "adaptive grid",
  "normal", or "grid with gradients".

#### Returns

An object of type `EvidenceSynthesisSource`. Create parameters for a
random-effects meta-analysis

------------------------------------------------------------------------

### Method `createRandomEffectsMetaAnalysis()`

#### Usage

    EvidenceSynthesisModule$createRandomEffectsMetaAnalysis(
      alpha = 0.05,
      evidenceSynthesisAnalysisId = 1,
      evidenceSynthesisDescription = "Random-effects",
      evidenceSynthesisSource = NULL,
      controlType = "outcome"
    )

#### Arguments

- `alpha`:

  The alpha (expected type I error) used for the confidence intervals.

- `evidenceSynthesisAnalysisId`:

  description

- `evidenceSynthesisDescription`:

  description

- `evidenceSynthesisSource`:

  description

- `controlType`:

  description Create a parameter object for the function
  computeFixedEffectMetaAnalysis

#### Details

Use DerSimonian-Laird meta-analysis

------------------------------------------------------------------------

### Method `createFixedEffectsMetaAnalysis()`

#### Usage

    EvidenceSynthesisModule$createFixedEffectsMetaAnalysis(
      alpha = 0.05,
      evidenceSynthesisAnalysisId = 1,
      evidenceSynthesisDescription = "Fixed-effects",
      evidenceSynthesisSource = NULL,
      controlType = "outcome"
    )

#### Arguments

- `alpha`:

  The alpha (expected type I error) used for the confidence intervals.

- `evidenceSynthesisAnalysisId`:

  description

- `evidenceSynthesisDescription`:

  description

- `evidenceSynthesisSource`:

  description

- `controlType`:

  description Create a parameter object for the function
  computeBayesianMetaAnalysis

#### Details

Create an object defining the parameter values.

------------------------------------------------------------------------

### Method `createBayesianMetaAnalysis()`

#### Usage

    EvidenceSynthesisModule$createBayesianMetaAnalysis(
      chainLength = 1100000,
      burnIn = 1e+05,
      subSampleFrequency = 100,
      priorSd = c(2, 0.5),
      alpha = 0.05,
      robust = FALSE,
      df = 4,
      seed = 1,
      evidenceSynthesisAnalysisId = 1,
      evidenceSynthesisDescription = "Bayesian random-effects",
      evidenceSynthesisSource = NULL,
      controlType = "outcome"
    )

#### Arguments

- `chainLength`:

  Number of MCMC iterations.

- `burnIn`:

  Number of MCMC iterations to consider as burn in.

- `subSampleFrequency`:

  Subsample frequency for the MCMC.

- `priorSd`:

  A two-dimensional vector with the standard deviation of the prior for
  mu and tau, respectively.

- `alpha`:

  The alpha (expected type I error) used for the credible intervals.

- `robust`:

  Whether or not to use a t-distribution model; default: FALSE.

- `df`:

  Degrees of freedom for the t-model, only used if robust is TRUE.

- `seed`:

  The seed for the random number generator.

- `evidenceSynthesisAnalysisId`:

  description

- `evidenceSynthesisDescription`:

  description

- `evidenceSynthesisSource`:

  description

- `controlType`:

  description Create EvidenceSynthesis diagnostics thresholds

#### Details

Create an object defining the parameter values.

------------------------------------------------------------------------

### Method `createEsDiagnosticThresholds()`

Threshold used to determine if we pass or fail diagnostics.

#### Usage

    EvidenceSynthesisModule$createEsDiagnosticThresholds(
      mdrrThreshold = 10,
      easeThreshold = 0.25,
      i2Threshold = 0.4,
      tauThreshold = log(2),
      sdmThreshold = NULL,
      sdmAlpha = NULL
    )

#### Arguments

- `mdrrThreshold`:

  What is the maximum allowed minimum detectable relative risk (MDRR)?

- `easeThreshold`:

  What is the maximum allowed expected absolute systematic error (EASE).

- `i2Threshold`:

  What is the maximum allowed I^2 (measure of between-database
  heterogeneity in random-effects models)?

- `tauThreshold`:

  What is the maximum allowed tau (measure of between-database
  heterogeneity in Bayesian random-effects models)?

- `sdmThreshold`:

  What is the maximum allowed standardized difference of mean (SDM) in a
  meta-analysis of balance statistics? If any covariate has a
  meta-analysis SDM exceeding this threshold, the diagnostic will fail.
  If set to NULL, this diagnostic will be ignored.

- `sdmAlpha`:

  What is the alpha for testing whether the absolute SDM exceeds
  `sdmThreshold`? If not provided, no significance testing will be
  performed and any absolute SDM greater than the threshold will be
  considered imbalance. Note that a Bonferroni adjustment will
  automatically be applied to adjust for the number of tests performed.

#### Returns

An object of type `EsDiagnosticThresholds`.

------------------------------------------------------------------------

### Method `createModuleSpecifications()`

Creates the module Specifications

#### Usage

    EvidenceSynthesisModule$createModuleSpecifications(
      evidenceSynthesisAnalysisList,
      esDiagnosticThresholds = self$createEsDiagnosticThresholds()
    )

#### Arguments

- `evidenceSynthesisAnalysisList`:

  A list of objects of type `EvidenceSynthesisAnalysis` as generated by
  either the
  [`EvidenceSynthesisModule$createFixedEffectsMetaAnalysis()`](https://ohdsi.github.io/Strategus/html/EvidenceSynthesisModule.html#method-createFixedEffectsMetaAnalysis)
  or
  [`EvidenceSynthesisModule$createBayesianMetaAnalysis()`](https://ohdsi.github.io/Strategus/html/EvidenceSynthesisModule.html#method-createBayesianMetaAnalysis)
  function.

- `esDiagnosticThresholds`:

  An object of type`EsDiagnosticThresholds` as generated by the
  [`EvidenceSynthesisModule$createEsDiagnosticThresholds()`](https://ohdsi.github.io/Strategus/html/EvidenceSynthesisModule.html#method-createEsDiagnosticThresholds)
  function.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EvidenceSynthesisModule$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
