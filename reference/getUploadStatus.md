# Inspect upload status

Inspects destination-specific module upload status without connecting to
the results database.

## Usage

``` r
getUploadStatus(
  analysisSpecifications,
  resultsDataModelSettings,
  resultsConnectionDetails,
  modules = NULL
)
```

## Arguments

- analysisSpecifications:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- resultsDataModelSettings:

  The results data model settings as created using
  [`createResultsDataModelSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsDataModelSettings.md).

- resultsConnectionDetails:

  The connection details to the results database which is an object of
  class `connectionDetails` as created by the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

- modules:

  Optional character vector selecting modules from the analysis
  specification. Matching is case-insensitive.

## Value

An `OperationStatus` object.
