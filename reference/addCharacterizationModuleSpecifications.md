# Add Characterization module specifications to analysis specifications

Add Characterization module specifications to analysis specifications

## Usage

``` r
addCharacterizationModuleSpecifications(
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
  [`CharacterizationModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/CharacterizationModule.html#method-CharacterizationModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
