# Create Results execution settings

Create Results execution settings

## Usage

``` r
createResultsExecutionSettings(
  resultsDatabaseSchema,
  workFolder,
  resultsFolder,
  logFileName = file.path(resultsFolder, "strategus-log.txt"),
  minCellCount = 5,
  maxCores = parallel::detectCores(),
  modulesToExecute = c()
)
```

## Arguments

- resultsDatabaseSchema:

  The schema in the results database that holds the results data model.

- workFolder:

  A folder in the local file system where intermediate results can be
  written.

- resultsFolder:

  The root folder holding the study results.

- logFileName:

  Logging information from Strategus and all modules will be located in
  this file. Individual modules will continue to have their own
  module-specific logs. By default this will be written to the root of
  the `resultsFolder`

- minCellCount:

  The minimum number of subjects contributing to a count before it can
  be included in results.

- maxCores:

  The maximum number of processing cores to use for execution. The
  default is to use all available cores on the machine.

- modulesToExecute:

  (Optional) A vector with the list of modules to execute. When an empty
  vector/NULL is supplied (default), all modules in the analysis
  specification are executed.

## Value

An object of type `ExecutionSettings`.
