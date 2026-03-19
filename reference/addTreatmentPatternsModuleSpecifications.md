# Add Treatment Patterns Module specifications to analysis specifications

Add Treatment Patterns Module specifications to analysis specifications

## Usage

``` r
addTreatmentPatternsModuleSpecifications(
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
  [`TreatmentPatternsModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/TreatmentPatternsModule.html#method-TreatmentPatternsModule-createModuleSpecifications)

## Value

Returns the `analysisSpecifications` object with the module
specifications added
