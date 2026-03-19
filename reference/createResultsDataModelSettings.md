# Create Results Data Model Settings

The results data model settings are used to create the results data
model and to upload results.

## Usage

``` r
createResultsDataModelSettings(
  resultsDatabaseSchema,
  resultsFolder,
  logFileName = file.path(resultsFolder, "strategus-results-data-model-log.txt"),
  modulesToExecute = c()
)
```

## Arguments

- resultsDatabaseSchema:

  The schema in the results database that holds the results data model.

- resultsFolder:

  The root folder holding the study results.

- logFileName:

  Log location for data model operations

- modulesToExecute:

  (Optional) A vector with the list of modules to execute. When an empty
  vector/NULL is supplied (default), all modules in the analysis
  specification are executed.

## Value

An object of type `ResultsDataModelSettings`
