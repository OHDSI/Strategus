# Add Patient Level Prediction module specifications to analysis specifications

Add Patient Level Prediction module specifications to analysis
specifications

## Usage

``` r
addPatientLevelPredictionModuleSpecifications(
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
  [`PatientLevelPredictionModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/PatientLevelPredictionModule.html#method-PatientLevelPredictionModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
