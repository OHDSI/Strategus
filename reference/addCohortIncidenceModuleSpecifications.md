# Add Cohort Incidence module specifications to analysis specifications

Add Cohort Incidence module specifications to analysis specifications

## Usage

``` r
addCohortIncidenceModuleSpecifications(
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
  [`CohortIncidenceModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/CohortIncidenceModule.html#method-CohortIncidenceModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
