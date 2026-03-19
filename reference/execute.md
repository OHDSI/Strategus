# Execute analysis specifications.

Execute analysis specifications.

## Usage

``` r
execute(analysisSpecifications, executionSettings, connectionDetails)
```

## Arguments

- analysisSpecifications:

  An object of type `AnalysisSpecifications` as created by
  [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md).

- executionSettings:

  An object of type `ExecutionSettings` as created by
  [`createCdmExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createCdmExecutionSettings.md)
  or
  [`createResultsExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsExecutionSettings.md).

- connectionDetails:

  An object of class `connectionDetails` as created by the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.

## Value

Returns a list of lists that contains

- moduleName: The name of the module executed

- result: The result of the execution. See purrr::safely for details on
  this result.

- executionTime: The time for the module to execute
