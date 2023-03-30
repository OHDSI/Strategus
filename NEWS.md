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
