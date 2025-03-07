ableToRunTreatmentPatterns <- function() {
  all(
    require("CirceR", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("CohortGenerator", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("DatabaseConnector", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("SqlRender", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("Eunomia", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  )
}

# NOTE: This function is used in the tests and in the package maintenance
# to create the settings which includes other cohorts. So naming this
# to allow for appending cohorts to an existing cohort set, if needed
appendTreatmentPatternsCohorts <- function(cohortDefinitionSet = CohortGenerator::createEmptyCohortDefinitionSet()) {
  cohortJsonFiles <- list.files(
    system.file(
      package = "TreatmentPatterns",
      "exampleCohorts"),
    full.names = TRUE)

  # NOTE: If this function is called with a non-empty
  # cohortDefinitionSet then we want to append the new cohorts
  # and not create it from scratch
  cdsRows <- nrow(cohortDefinitionSet)
  for (i in seq_len(length(cohortJsonFiles))) {
    cohortJsonFileName <- cohortJsonFiles[i]
    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
    cohortJson <- readChar(cohortJsonFileName, file.info(
      cohortJsonFileName)$size)

    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)

    cohortSql <- CirceR::buildCohortQuery(
      cohortExpression,
      options = CirceR::createGenerateOptions(generateStats = FALSE))
    cohortDefinitionSet <- rbind(
      cohortDefinitionSet,
      data.frame(
        cohortId = (cdsRows + i),
        cohortName = cohortName,
        sql = cohortSql,
        json = cohortJson,
        stringsAsFactors = FALSE))
  }

  return(cohortDefinitionSet)
}

getTreatmentPatternsCohorts <- function(cohortDefinitionSet = CohortGenerator::createEmptyCohortDefinitionSet()) {
  # Select Viral Sinusitis Cohort
  targetCohorts <- cohortDefinitionSet |>
    dplyr::filter(cohortName == "ViralSinusitis") |>
    dplyr::select(cohortId, cohortName)

  # Select everything BUT Viral Sinusitis cohorts
  eventCohorts <- cohortDefinitionSet |>
    dplyr::filter(cohortName != "ViralSinusitis" & cohortName != "Death") |>
    dplyr::select(cohortId, cohortName)

  exitCohorts <- cohortDefinitionSet |>
    dplyr::filter(cohortName == "Death") |>
    dplyr::select(cohortId, cohortName)

  cohorts <- dplyr::bind_rows(
    targetCohorts |> dplyr::mutate(type = "target"),
    eventCohorts |> dplyr::mutate(type = "event"),
    exitCohorts |> dplyr::mutate(type = "exit")
  )
  return(cohorts)
}

generateCohortTable <- function() {
  if (ableToRunTreatmentPatterns()) {
    connectionDetails <- Eunomia::getEunomiaConnectionDetails()
    cohortTableName <- "cohort_table"
    resultSchema <- "main"
    cdmSchema <- "main"

    cohortsToCreate <- appendTreatmentPatternsCohorts()

    cohortTableNames <- CohortGenerator::getCohortTableNames(
      cohortTable = cohortTableName)

    CohortGenerator::createCohortTables(
      connectionDetails = connectionDetails,
      cohortDatabaseSchema = resultSchema,
      cohortTableNames = cohortTableNames)

    # Generate the cohorts
    cohortsGenerated <- CohortGenerator::generateCohortSet(
      connectionDetails = connectionDetails,
      cdmDatabaseSchema = cdmSchema,
      cohortDatabaseSchema = resultSchema,
      cohortTableNames = cohortTableNames,
      cohortDefinitionSet = cohortsToCreate)

    cohorts <- getTreatmentPatternsCohorts(cohortsToCreate)

    return(list(
      cohorts = cohorts,
      connectionDetails = connectionDetails,
      cohortTableName = cohortTableName,
      resultSchema = resultSchema,
      cdmSchema = cdmSchema
    ))
  } else {
    return(NULL)
  }
}
