# Gets the metadata for your OMOP CDM Database

This function is used to gather metadata about your OMOP CDM and inspect
for informational purposes. This information will be saved with your
results when executing an analysis specification.

## Usage

``` r
getCdmDatabaseMetaData(cdmExecutionSettings, connectionDetails)
```

## Arguments

- cdmExecutionSettings:

  An object of type `CdmExecutionSettings` as created
  [`createCdmExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createCdmExecutionSettings.md).

- connectionDetails:

  An object of class `connectionDetails` as created by the
  [`DatabaseConnector::createConnectionDetails()`](https://ohdsi.github.io/DatabaseConnector/reference/createConnectionDetails.html)
  function.
