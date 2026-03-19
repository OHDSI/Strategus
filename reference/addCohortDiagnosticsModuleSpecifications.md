# Add Cohort Diagnostics module specifications to analysis specifications

Add Cohort Diagnostics module specifications to analysis specifications

## Usage

``` r
addCohortDiagnosticsModuleSpecifications(
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
  [`CohortDiagnosticsModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/CohortDiagnosticsModule.html#method-CohortDiagnosticsModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
