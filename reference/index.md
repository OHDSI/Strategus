# Package index

## Create analysis specifications

Functions for creating the analysis specification to execute. An
analysis specification includes one or more Strategus HADES module
settings which are added to the analysis specification.

- [`addCharacterizationModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addCharacterizationModuleSpecifications.md)
  : Add Characterization module specifications to analysis
  specifications
- [`addCohortDiagnosticsModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addCohortDiagnosticsModuleSpecifications.md)
  : Add Cohort Diagnostics module specifications to analysis
  specifications
- [`addCohortGeneratorModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addCohortGeneratorModuleSpecifications.md)
  : Add Cohort Generator module specifications to analysis
  specifications
- [`addCohortIncidenceModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addCohortIncidenceModuleSpecifications.md)
  : Add Cohort Incidence module specifications to analysis
  specifications
- [`addCohortMethodeModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addCohortMethodeModuleSpecifications.md)
  : Add Cohort Method module specifications to analysis specifications
- [`addEvidenceSynthesisModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addEvidenceSynthesisModuleSpecifications.md)
  : Add Evidence Synthesis module specifications to analysis
  specifications
- [`addModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addModuleSpecifications.md)
  : Add generic module specifications to analysis specifications
- [`addPatientLevelPredictionModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addPatientLevelPredictionModuleSpecifications.md)
  : Add Patient Level Prediction module specifications to analysis
  specifications
- [`addPatientLevelPredictionValidationModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addPatientLevelPredictionValidationModuleSpecifications.md)
  : Add Patient Level Prediction Validation Module module specifications
  to analysis specifications
- [`addSelfControlledCaseSeriesModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addSelfControlledCaseSeriesModuleSpecifications.md)
  : Add Self Controlled Case Series Module module specifications to
  analysis specifications
- [`addTreatmentPatternsModuleSpecifications()`](https://ohdsi.github.io/Strategus/reference/addTreatmentPatternsModuleSpecifications.md)
  : Add Treatment Patterns Module specifications to analysis
  specifications
- [`addSharedResources()`](https://ohdsi.github.io/Strategus/reference/addSharedResources.md)
  : Add shared resources (i.e. cohorts) to analysis specifications
- [`createEmptyAnalysisSpecifications()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecifications.md)
  : Create an empty analysis specifications object.
- [`createEmptyAnalysisSpecificiations()`](https://ohdsi.github.io/Strategus/reference/createEmptyAnalysisSpecificiations.md)
  : DEPRECATED DUE TO MISSPELLING

## Executing a study

These functions are used to create the settings necessary to execute a
study as defined by the contents of the analysis specification.

- [`execute()`](https://ohdsi.github.io/Strategus/reference/execute.md)
  : Execute analysis specifications.
- [`createCdmExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createCdmExecutionSettings.md)
  : Create CDM execution settings
- [`createResultsExecutionSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsExecutionSettings.md)
  : Create Results execution settings
- [`getCdmDatabaseMetaData()`](https://ohdsi.github.io/Strategus/reference/getCdmDatabaseMetaData.md)
  : Gets the metadata for your OMOP CDM Database

## HADES Modules

### OMOP CDM HADES Modules

These modules are designed to run against your OMOP CDM to carry out
their analyses.

- [`CharacterizationModule`](https://ohdsi.github.io/Strategus/reference/CharacterizationModule.md)
  :

  Characterize cohorts with the [HADES Characterization
  Package](https://ohdsi.github.io/Characterization/)

- [`CohortDiagnosticsModule`](https://ohdsi.github.io/Strategus/reference/CohortDiagnosticsModule.md)
  :

  Evaluate phenotypes with the [HADES CohortDiagnostics
  Package](https://ohdsi.github.io/CohortDiagnostics/)

- [`CohortGeneratorModule`](https://ohdsi.github.io/Strategus/reference/CohortGeneratorModule.md)
  :

  Generate cohorts with the [HADES CohortGenerator
  Package](https://ohdsi.github.io/CohortGenerator/)

- [`CohortIncidenceModule`](https://ohdsi.github.io/Strategus/reference/CohortIncidenceModule.md)
  :

  Compute incidence with the [HADES CohortIncidence
  Package](https://ohdsi.github.io/CohortIncidence/)

- [`CohortMethodModule`](https://ohdsi.github.io/Strategus/reference/CohortMethodModule.md)
  :

  New-user cohort studies with the [HADES CohortMethod
  Package](https://ohdsi.github.io/CohortMethod/)

- [`PatientLevelPredictionModule`](https://ohdsi.github.io/Strategus/reference/PatientLevelPredictionModule.md)
  :

  Patient-level prediction with the [HADES PatientLevelPrediction
  Package](https://ohdsi.github.io/PatientLevelPrediction/)

- [`PatientLevelPredictionValidationModule`](https://ohdsi.github.io/Strategus/reference/PatientLevelPredictionValidationModule.md)
  :

  Validation of patient-level prediction models with the [HADES
  PatientLevelPrediction
  Package](https://ohdsi.github.io/PatientLevelPrediction/)

- [`SelfControlledCaseSeriesModule`](https://ohdsi.github.io/Strategus/reference/SelfControlledCaseSeriesModule.md)
  :

  Self-Controlled Case Series design with the [HADES
  SelfControlledCaseSeries
  Package](https://ohdsi.github.io/SelfControlledCaseSeries/)

- [`TreatmentPatternsModule`](https://ohdsi.github.io/Strategus/reference/TreatmentPatternsModule.md)
  :

  Evaluate phenotypes with the [DARWIN TreatmentPatterns
  Package](https://github.com/darwin-eu/TreatmentPatterns/)

### Results HADES Modules

These modules are designed to run against results that are produced by
one or more OMOP CDM HADES Modules. Results must be stored in a
PostgreSQL database.

- [`EvidenceSynthesisModule`](https://ohdsi.github.io/Strategus/reference/EvidenceSynthesisModule.md)
  :

  Meta-analysis with the [HADES EvidenceSynthesis
  Package](https://ohdsi.github.io/EvidenceSynthesis/)

## Results Data Model Creation & Upload

These functions are used to create the PostgreSQL results data model
tables and to upload results produced by the HADES modules.

- [`createResultDataModel()`](https://ohdsi.github.io/Strategus/reference/createResultDataModel.md)
  : Create Result Data Model
- [`createResultsDataModelSettings()`](https://ohdsi.github.io/Strategus/reference/createResultsDataModelSettings.md)
  : Create Results Data Model Settings
- [`uploadResults()`](https://ohdsi.github.io/Strategus/reference/uploadResults.md)
  : Upload results

## Sharing results

These functions are used to prepare results to share with a network
study coordinator

- [`zipResults()`](https://ohdsi.github.io/Strategus/reference/zipResults.md)
  : Create a zip file with all study results for sharing with study
  coordinator

## Internal R6 Class

This is a list of internal classes used by Strategus developers

- [`StrategusModule`](https://ohdsi.github.io/Strategus/reference/StrategusModule.md)
  : StrategusModule defines the base class for each HADES module
