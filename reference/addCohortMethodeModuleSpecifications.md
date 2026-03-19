# Add Cohort Method module specifications to analysis specifications

Add Cohort Method module specifications to analysis specifications

## Usage

``` r
addCohortMethodeModuleSpecifications(
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
  [`CohortMethodModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/CohortMethodModule.html#method-CohortMethodModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
