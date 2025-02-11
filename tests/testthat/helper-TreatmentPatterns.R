ableToRunTreatmentPatterns <- function() {
  all(
    require("CirceR", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("CohortGenerator", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("DatabaseConnector", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("SqlRender", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
    require("Eunomia", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  )
}

generateCohortTable <- function() {
  if (ableToRunTreatmentPatterns()) {
    connectionDetails <- Eunomia::getEunomiaConnectionDetails()
    cohortTableName <- "cohort_table"
    resultSchema <- "main"
    cdmSchema <- "main"

    cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()

    cohortJsonFiles <- list.files(
      system.file(
        package = "TreatmentPatterns",
        "exampleCohorts"),
      full.names = TRUE)

    for (i in seq_len(length(cohortJsonFiles))) {
      cohortJsonFileName <- cohortJsonFiles[i]
      cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
      cohortJson <- readChar(cohortJsonFileName, file.info(
        cohortJsonFileName)$size)

      cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)

      cohortSql <- CirceR::buildCohortQuery(
        cohortExpression,
        options = CirceR::createGenerateOptions(generateStats = FALSE))
      cohortsToCreate <- rbind(
        cohortsToCreate,
        data.frame(
          cohortId = i,
          cohortName = cohortName,
          sql = cohortSql,
          stringsAsFactors = FALSE))
    }

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

    # Select Viral Sinusitis Cohort
    targetCohorts <- cohortsGenerated %>%
      dplyr::filter(cohortName == "ViralSinusitis") %>%
      dplyr::select(cohortId, cohortName)

    # Select everything BUT Viral Sinusitis cohorts
    eventCohorts <- cohortsGenerated %>%
      dplyr::filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
      dplyr::select(cohortId, cohortName)

    exitCohorts <- cohortsGenerated %>%
      dplyr::filter(cohortName == "Death") %>%
      dplyr::select(cohortId, cohortName)

    cohorts <- dplyr::bind_rows(
      targetCohorts %>% dplyr::mutate(type = "target"),
      eventCohorts %>% dplyr::mutate(type = "event"),
      exitCohorts %>% dplyr::mutate(type = "exit")
    )

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
