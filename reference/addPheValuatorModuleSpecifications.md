# Add PheValuator Module specifications to analysis specifications

Add PheValuator Module specifications to analysis specifications

## Usage

``` r
addPheValuatorModuleSpecifications(
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
  [`PheValuatorModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/PheValuatorModule.html#method-PheValuatorModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
