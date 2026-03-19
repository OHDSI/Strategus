# Changelog

## Strategus 1.5.0

New Features

- Update CohortMethod module to support CohortMethod v6
  ([\#277](https://github.com/OHDSI/Strategus/issues/277))
- Update Characterization module to support Characterization v3
  ([\#288](https://github.com/OHDSI/Strategus/issues/288))
- Add prediction intervals to EvidenceSynthesisModule
  ([\#276](https://github.com/OHDSI/Strategus/issues/276))
- Add covariate balance meta-analysis to EvidenceSynthesisModule
  ([\#275](https://github.com/OHDSI/Strategus/issues/275))
- Add analysisId and stratify options to TreatmentPatterns
  ([\#271](https://github.com/OHDSI/Strategus/issues/271))
- Add support for cohort templates
  ([\#242](https://github.com/OHDSI/Strategus/issues/242))
- createResultDataModel should always create the databaseMetadata table
  ([\#232](https://github.com/OHDSI/Strategus/issues/232))

Fixes

- Uploading fails when results.zip is missing for CM/SCCS
  ([\#263](https://github.com/OHDSI/Strategus/issues/263))

## Strategus 1.4.1

- Patch for PatientLevelPredictionModule &
  PatientLevelPredictionValidationModule to handle covariate cohort
  tables ([\#227](https://github.com/OHDSI/Strategus/issues/227))

## Strategus 1.4.0

- Update TreatmentPatterns module to support \>= v3.1.0
  ([\#233](https://github.com/OHDSI/Strategus/issues/233))
- Update SelfControlledCaseSeries module to support \>= v6.0.0
  ([\#231](https://github.com/OHDSI/Strategus/issues/231))
- Update PatientLevelPrediction module to support \>= v6.5.0 and to skip
  diagnostics ([\#247](https://github.com/OHDSI/Strategus/issues/247))
- Adds more flexible analyses options for Characterization module
  ([\#248](https://github.com/OHDSI/Strategus/issues/248))
- Fix spelling of `createEmptyAnalysisSpecifications` function
  ([\#249](https://github.com/OHDSI/Strategus/issues/249))
- Update CohortIncidence module to use lower case columns to support
  upcoming DatabaseConnector v7
  ([\#244](https://github.com/OHDSI/Strategus/issues/244))
- Support new likelihood profiling (grid with gradients using Hermite
  interpolation) in EvidenceSynthesis v1.0
  ([\#243](https://github.com/OHDSI/Strategus/issues/243))

## Strategus 1.3.1

- EvidenceSynthesis results model - make target/comparator days bigint
  ([\#212](https://github.com/OHDSI/Strategus/issues/212))
- EvidenceSynthesis results model - make observed_days bigint
  ([\#221](https://github.com/OHDSI/Strategus/issues/221))
- Not using unblind_for_evidence_synthesis even if exists
  ([\#218](https://github.com/OHDSI/Strategus/issues/218))
- CohortDiagnosticsModule bug with cohort ID specification
  ([\#234](https://github.com/OHDSI/Strategus/issues/234))
- `tempEmulationSchema` not being passed to all modules
  ([\#229](https://github.com/OHDSI/Strategus/issues/229))
- CohortDiagnostics incremental flag not passed
  ([\#237](https://github.com/OHDSI/Strategus/issues/237))

## Strategus 1.3.0

New Features

- Adds TreatmentPatterns module (experimental)
  ([\#186](https://github.com/OHDSI/Strategus/issues/186))
- Adds PatientLevelPredictionValidation module (experimental)
  ([\#164](https://github.com/OHDSI/Strategus/issues/164))

Fixes

- Example analysis specification modifications for better testing
  ([\#210](https://github.com/OHDSI/Strategus/issues/210))
- Remove redundant messages in the execution summary
  ([\#213](https://github.com/OHDSI/Strategus/issues/213))

## Strategus 1.2.0

- Provide summary information when creating the results data model and
  uploading results
  ([\#204](https://github.com/OHDSI/Strategus/issues/204))
- Add error handling when obtaining OMOP CDM metadata
  ([\#201](https://github.com/OHDSI/Strategus/issues/201))
- Strategus execution should stop if an error occurs during cohort
  generation ([\#189](https://github.com/OHDSI/Strategus/issues/189))
- Support `modulesToExecute` parameter for results model creation &
  results upload
  ([\#177](https://github.com/OHDSI/Strategus/issues/177))
- Better handling of incremental execution of Characterization
  ([\#194](https://github.com/OHDSI/Strategus/issues/194))
- Ensure logging captures all output
  ([\#196](https://github.com/OHDSI/Strategus/issues/196))

## Strategus 1.1.2

- Add options for overriding number of threads for database operations
  ([\#190](https://github.com/OHDSI/Strategus/issues/190))

## Strategus 1.1.1

- Fixes R CMD note and updates documentation for inclusion into HADES

## Strategus 1.1.0

- Inject cohort schema and table into
  `createCohortBasedCovariateSettings`
  ([\#181](https://github.com/OHDSI/Strategus/issues/181))
- Provide hook to optimize cohort SQL construction
  ([\#179](https://github.com/OHDSI/Strategus/issues/179))
- Fixes broken links in R6 class documentation
  ([\#183](https://github.com/OHDSI/Strategus/issues/183))

## Strategus 1.0.0

- Eliminated the dependencies for `keyring`, `renv` and `targets`
  ([\#135](https://github.com/OHDSI/Strategus/issues/135))
- Centralized all modules into the Strategus package
  ([\#29](https://github.com/OHDSI/Strategus/issues/29),
  [\#45](https://github.com/OHDSI/Strategus/issues/45),
  [\#51](https://github.com/OHDSI/Strategus/issues/51),
  [\#140](https://github.com/OHDSI/Strategus/issues/140))
- Strategus execution provides a summary of the execution time
  ([\#4](https://github.com/OHDSI/Strategus/issues/4)) and continues
  after an error ([\#64](https://github.com/OHDSI/Strategus/issues/64),
  [\#141](https://github.com/OHDSI/Strategus/issues/141))
- Modules provide functions for creating their results data model
  ([\#43](https://github.com/OHDSI/Strategus/issues/43)) and to upload
  results.
- Allow modules to obtain the full set of OMOP CDM metadata collected
  ([\#20](https://github.com/OHDSI/Strategus/issues/20),
  [\#154](https://github.com/OHDSI/Strategus/issues/154))
- Adds the `cdm_holder` attribute to the database_id hash calculation
  ([\#48](https://github.com/OHDSI/Strategus/issues/48))
- Moves the `incremental` setting to the execution settings vs. the
  module settings
  ([\#155](https://github.com/OHDSI/Strategus/issues/155))
- Adds threading options via execution settings
  ([\#151](https://github.com/OHDSI/Strategus/issues/151))
- Select subset of modules to execute from analysis specification
  ([\#169](https://github.com/OHDSI/Strategus/issues/169))
- Ensure release of Strategus has Python dependencies
  ([\#22](https://github.com/OHDSI/Strategus/issues/22)) and OHDSI Shiny
  App dependencies
  ([\#78](https://github.com/OHDSI/Strategus/issues/78)). See
  [StrategusStudyRepoTemplate](https://github.com/ohdsi-studies/StrategusStudyRepoTemplate)
  for more details.
- Document the results data model for HADES modules
  ([\#143](https://github.com/OHDSI/Strategus/issues/143))

### Bug Fixes

- Make negative control outcome shared resource optional
  ([\#153](https://github.com/OHDSI/Strategus/issues/153))
- Export results data model specification file for
  PatientLevelPrediction
  ([\#165](https://github.com/OHDSI/Strategus/issues/165))

## Strategus 0.3.0

- Provide option to skip modules
  ([\#87](https://github.com/OHDSI/Strategus/issues/87))
- Central log file for execution
  ([\#132](https://github.com/OHDSI/Strategus/issues/132))
- Create function to collect all results into a single ZIP file for
  sharing ([\#46](https://github.com/OHDSI/Strategus/issues/46))
- Install latest modules
  ([\#125](https://github.com/OHDSI/Strategus/issues/125))

## Strategus 0.2.1

- Update SelfControlledCaseSeries Module to v0.4.1

## Strategus 0.2.0

- Add functions for developers to help with renv.lock file validation
  ([\#69](https://github.com/OHDSI/Strategus/issues/69))
- Use renv project profiles for modules
  ([\#94](https://github.com/OHDSI/Strategus/issues/94))
- Convert relative paths to absolute path before passing to a module
  ([\#99](https://github.com/OHDSI/Strategus/issues/99))
- Address missing package dependencies in modules
  ([\#99](https://github.com/OHDSI/Strategus/issues/99))
- Throw informative error message when connection detail reference not
  set in keyring
  ([\#100](https://github.com/OHDSI/Strategus/issues/100))
- Validate execution settings
  ([\#101](https://github.com/OHDSI/Strategus/issues/101))
- Pass temp emulation schema properly
  ([\#76](https://github.com/OHDSI/Strategus/issues/76))
- Remove local library package dependencies
  ([\#96](https://github.com/OHDSI/Strategus/issues/96))

## Strategus 0.1.0

- Adds an initial implementation for uploading results to a results
  database ([\#72](https://github.com/OHDSI/Strategus/issues/72))
- Robust handling of connection details via keyring
  ([\#74](https://github.com/OHDSI/Strategus/issues/74))
- Ensures uniqueness of all CDM tables when gathering database metadata
  ([\#82](https://github.com/OHDSI/Strategus/issues/82))
- `tempEmulationSchema` added to execution settings and passed properly
  to modules ([\#82](https://github.com/OHDSI/Strategus/issues/82))
- Adding logging to module initialization to detect `renv` restore
  errors ([\#82](https://github.com/OHDSI/Strategus/issues/82))
- Adopt HADES-wide lock file in latest versions of all modules
  ([\#83](https://github.com/OHDSI/Strategus/issues/83))
- Use renv \>= v1.0.0 for all modules and Strategus
  ([\#83](https://github.com/OHDSI/Strategus/issues/83))
- Add GitHub unit tests for HADES adopted version (currently v4.2.3) and
  the latest R versions for all modules and Strategus.
  ([\#83](https://github.com/OHDSI/Strategus/issues/83))
- Ensure all Strategus GitHub unit tests run on all operating systems
  and available OHDSI test database platforms
  ([\#83](https://github.com/OHDSI/Strategus/issues/83))
- Use CDM v5.4 schemas for all unit tests
  ([\#85](https://github.com/OHDSI/Strategus/issues/85))
- Allow for passing `renv`configuration options when running Strategus
  ([\#88](https://github.com/OHDSI/Strategus/issues/88))
- Adds SQL for test cohorts to package
  ([\#1](https://github.com/OHDSI/Strategus/issues/1))

## Strategus 0.0.6

- Update SCCS module reference
  `inst/testdata/analysisSpecification.json`

## Strategus 0.0.5

- Required metadata tables check fails with DatabaseConnector \< 6.0
  ([\#61](https://github.com/OHDSI/Strategus/issues/61))
- Update module references and add script to run Strategus on Eunomia in
  `extras/ExecuteStrategusOnEunomia.R`
  ([\#66](https://github.com/OHDSI/Strategus/issues/66))

## Strategus 0.0.4

- Add DB Platform Tests
  ([\#53](https://github.com/OHDSI/Strategus/issues/53))
- Add error handling for missing/empty tables
  ([\#54](https://github.com/OHDSI/Strategus/issues/54))
- Remove uniqueness check for module table prefix
  ([\#55](https://github.com/OHDSI/Strategus/issues/55))

## Strategus 0.0.3

- Breaking change: removed function `createExecutionSettings()` and
  replaced with 2 new functions:
  [`createCdmExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createCdmExecutionSettings.md)
  and
  [`createResultsExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsExecutionSettings.md).
  ([\#19](https://github.com/OHDSI/Strategus/issues/19))
- Added Vignettes ([\#23](https://github.com/OHDSI/Strategus/issues/23))
- Provide better support for `keyring` to handle named/locked keyrings
  ([\#24](https://github.com/OHDSI/Strategus/issues/24))
- Add function to list HADES modules
  ([\#30](https://github.com/OHDSI/Strategus/issues/30))
- Fixes from testing
  ([\#36](https://github.com/OHDSI/Strategus/issues/36))
- Enforce module structure for proper use with renv
  ([\#37](https://github.com/OHDSI/Strategus/issues/37))
- Support CDM 5.4 source table format
  ([\#41](https://github.com/OHDSI/Strategus/issues/41))
- Add unit tests ([\#47](https://github.com/OHDSI/Strategus/issues/47))

## Strategus 0.0.2

- Updates renv to 0.15.5
- Call renv::use() for each module

## Strategus 0.0.1

Initial version
