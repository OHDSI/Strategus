# Add Self Controlled Case Series Module module specifications to analysis specifications

Add Self Controlled Case Series Module module specifications to analysis
specifications

## Usage

``` r
addSelfControlledCaseSeriesModuleSpecifications(
  analysisSpecifications,
  moduleSpecifications
)
```

## Arguments

- analysisSpecifications:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- moduleSpecifications:

  Created by the
  [`SelfControlledCaseSeriesModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/SelfControlledCaseSeriesModule.html#method-SelfControlledCaseSeriesModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
