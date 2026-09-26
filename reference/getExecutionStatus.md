# Inspect execution status

Inspects module execution status without executing the analysis or
connecting to a database.

## Usage

``` r
getExecutionStatus(analysisSpecifications, resultsFolder, modules = NULL)
```

## Arguments

- analysisSpecifications:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- resultsFolder:

  The root folder holding the study results.

- modules:

  Optional character vector selecting modules from the analysis
  specification. Matching is case-insensitive.

## Value

An `OperationStatus` object.
