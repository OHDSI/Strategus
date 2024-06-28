library(Strategus)

#m <- StrategusModule$new()
jc <- JobContext$new()

#m$execute(jc)
#m$createResultsSchema(NULL)

# Create the job context
library(CohortGenerator)
cohortDefinitionSet <- getCohortDefinitionSet(
  settingsFileName = "testdata/Cohorts.csv",
  jsonFolder = "testdata/cohorts",
  sqlFolder = "testdata/sql",
  packageName = "Strategus"
)
ncoCohortSet <- readCsv(file = system.file("testdata/negative_controls_concept_set.csv",
                                           package = "Strategus"
))

# NOTE: THIS SHOULD GO IN THE MODULE!
createCohortSharedResourceSpecifications <- function(cohortDefinitionSet) {
  if (!CohortGenerator::isCohortDefinitionSet(cohortDefinitionSet)) {
    stop("cohortDefinitionSet is not properly defined")
  }

  subsetDefinitions <- CohortGenerator::getSubsetDefinitions(cohortDefinitionSet)
  if (length(subsetDefinitions) > 0) {
    # Filter the cohort definition set to the "parent" cohorts.
    parentCohortDefinitionSet <- cohortDefinitionSet[!cohortDefinitionSet$isSubset, ]
  } else {
    parentCohortDefinitionSet <- cohortDefinitionSet
  }

  sharedResource <- list()

  listafy <- function(df) {
    mylist <- list()
    for (i in 1:nrow(df)) {
      cohortData <- list(
        cohortId = df$cohortId[i],
        cohortName = df$cohortName[i],
        cohortDefinition = df$json[i]
      )
      mylist[[i]] <- cohortData
    }
    return(mylist)
  }

  cohortDefinitionSetFiltered <- listafy(parentCohortDefinitionSet)
  sharedResource["cohortDefinitions"] <- list(cohortDefinitionSetFiltered)

  if (length(subsetDefinitions)) {
    # Subset definitions
    subsetDefinitionsJson <- lapply(subsetDefinitions, function(x) {
      x$toJSON()
    })
    sharedResource["subsetDefs"] <- list(subsetDefinitionsJson)

    # Filter to the subsets
    subsetCohortDefinitionSet <- cohortDefinitionSet[cohortDefinitionSet$isSubset, ]
    subsetIdMapping <- list()
    for (i in 1:nrow(subsetCohortDefinitionSet)) {
      idMapping <- list(
        cohortId = subsetCohortDefinitionSet$cohortId[i],
        subsetId = subsetCohortDefinitionSet$subsetDefinitionId[i],
        targetCohortId = subsetCohortDefinitionSet$subsetParent[i]
      )
      subsetIdMapping[[i]] <- idMapping
    }
    sharedResource["cohortSubsets"] <- list(subsetIdMapping)
  }


  class(sharedResource) <- c("CohortDefinitionSharedResources", "SharedResources")
  return(sharedResource)
}

# NOTE: THIS SHOULD GO IN THE MODULE!
createNegativeControlOutcomeCohortSharedResourceSpecifications <- function(negativeControlOutcomeCohortSet,
                                                                           occurrenceType,
                                                                           detectOnDescendants) {
  negativeControlOutcomeCohortSet <- apply(negativeControlOutcomeCohortSet, 1, as.list)
  sharedResource <- list(
    negativeControlOutcomes = list(
      negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
      occurrenceType = occurrenceType,
      detectOnDescendants = detectOnDescendants
    )
  )
  class(sharedResource) <- c("NegativeControlOutcomeSharedResources", "SharedResources")
  return(sharedResource)
}

jc$sharedResources <- list(
  createCohortSharedResourceSpecifications(cohortDefinitionSet),
  createNegativeControlOutcomeCohortSharedResourceSpecifications(ncoCohortSet, "first", TRUE)
)
jc$settings <- list(
  incremental = FALSE,
  generateStats = TRUE
)
jc$moduleExecutionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = "eunomia", # TODO: This needs to go
  workDatabaseSchema = "main",
  cdmDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(),
  workFolder = "work_folder",
  resultsFolder = "results_folder",
  minCellCount = 5,
  resultsConnectionDetailsReference = "eunomia",
  resultsDatabaseSchema = "main"
)

# Missing the database ID from the constructor!

connectionDetails <- Eunomia::getEunomiaConnectionDetails()

cg <- CohortGeneratorModule$new(
  jobContext = jc,
  moduleIndex = 1,
  databaseId = "foo"
)
#debugonce(cg$execute)
#debugonce(CohortGenerator:::generateAndExportNegativeControls)
cg$execute(
  connectionDetails = connectionDetails
)

debugonce(cg$exportResults)
cg$exportResults(
  connectionDetails = connectionDetails
)
#cg$createResultsSchema(NULL)
