# Add Patient Level Prediction Validation Module module specifications to analysis specifications

Add Patient Level Prediction Validation Module module specifications to
analysis specifications

## Usage

``` r
addPatientLevelPredictionValidationModuleSpecifications(
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
  [`PatientLevelPredictionValidationModule$createModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/PatientLevelPredictionValidationModule.html#method-PatientLevelPredictionValidationModule-createModuleSpecifications)
  function.

## Value

Returns the `analysisSpecifications` object with the module
specifications added.
