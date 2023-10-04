Strategus 0.1.0
===============

- Adds an initial implementation for uploading results to a results database (#72)
- Robust handling of connection details via keyring (#74)
- Ensures uniqueness of all CDM tables when gathering database metadata (#82)
- `tempEmulationSchema` added to execution settings and passed properly to modules (#82)
- Adding logging to module initalization to detect `renv` restore errors (#82)
- Adopt HADES-wide lock file in latest versions of all modules (#83)
- Use renv >= v1.0.0 for all modules and Strategus (#83)
- Add GitHub unit tests for HADES adopted version (currently v4.2.3) and the latest R versions for all modules and Strategus. (#83)
- Ensure all Strategus GitHub unit tests run on all operating systems and available OHDSI test database platforms (#83)
- Use CDM v5.4 schemas for all unit tests (#85)
- Allow for passing `renv`configuration options when running Strategus (#88)
- Adds SQL for test cohorts to package (#1)

Strategus 0.0.6
===============

- Update SCCS module reference `inst/testdata/analysisSpecification.json` 

Strategus 0.0.5
===============

- Required metadata tables check fails with DatabaseConnector < 6.0 (#61)
- Update module references and add script to run Strategus on Eunomia in `extras/ExecuteStrategusOnEunomia.R` (#66)

Strategus 0.0.4
===============

- Add DB Platform Tests (#53)
- Add error handling for missing/empty tables (#54)
- Remove uniqueness check for module table prefix (#55)

Strategus 0.0.3
===============

- Breaking change: removed function `createExecutionSettings()` and replaced with 2 new functions: `createCdmExecutionSettings()` and `createResultsExecutionSettings()`. (#19)
- Added Vignettes (#23)
- Provide better support for `keyring` to handle named/locked keyrings (#24)
- Add function to list Strategus modules (#30)
- Fixes from testing (#36)
- Enforce module structure for proper use with renv (#37)
- Support CDM 5.4 source table format (#41)
- Add unit tests (#47)


Strategus 0.0.2
===============

- Updates renv to 0.15.5
- Call renv::use() for each module


Strategus 0.0.1
===============

Initial version
