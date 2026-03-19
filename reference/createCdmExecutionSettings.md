# Create CDM execution settings

Create CDM execution settings

## Usage

``` r
createCdmExecutionSettings(
  workDatabaseSchema,
  cdmDatabaseSchema,
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
  tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
  workFolder,
  resultsFolder,
  logFileName = file.path(resultsFolder, "strategus-log.txt"),
  minCellCount = 5,
  incremental = TRUE,
  maxCores = parallel::detectCores(),
  modulesToExecute = c()
)
```

## Arguments

- workDatabaseSchema:

  A database schema where intermediate data can be stored. The user (as
  identified in the connection details) will need to have write access
  to this database schema.

- cdmDatabaseSchema:

  The database schema containing the data in CDM format. The user (as
  identified in the connection details) will need to have read access to
  this database schema.

- cohortTableNames:

  An object identifying the various cohort table names that will be
  created in the `workDatabaseSchema`. This object can be created using
  the
  [`CohortGenerator::getCohortTableNames()`](https://ohdsi.github.io/CohortGenerator/reference/getCohortTableNames.html)
  function.

- tempEmulationSchema:

  Some database platforms like Oracle and Impala do not truly support
  temp tables. To emulate temp tables, provide a schema with write
  privileges where temp tables can be created.

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

- incremental:

  This value will be passed to each module that supports execution in an
  incremental manner. Modules and their underlying packages may use the
  `workFolder` contents to determine their state of execution and
  attempt to pick up where they left off when this value is set to TRUE.

- maxCores:

  The maximum number of processing cores to use for execution. The
  default is to use all available cores on the machine.

- modulesToExecute:

  (Optional) A vector with the list of modules to execute. When an empty
  vector/NULL is supplied (default), all modules in the analysis
  specification are executed.

## Value

An object of type `ExecutionSettings`.
