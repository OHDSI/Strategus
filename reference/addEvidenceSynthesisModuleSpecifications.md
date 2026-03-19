# Add Evidence Synthesis module specifications to analysis specifications

Add Evidence Synthesis module specifications to analysis specifications

## Usage

``` r
addEvidenceSynthesisModuleSpecifications(
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
  [`EvidenceSynthesisModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/EvidenceSynthesisModule.html#method-EvidenceSynthesisModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
