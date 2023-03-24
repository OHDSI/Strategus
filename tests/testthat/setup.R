library(testthat)
library(Strategus)
library(Eunomia)
library(dplyr)

dbms <- getOption("dbms", default = "sqlite")
message("************* Testing on ", dbms, " *************")

if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- "~/.jdbcDrivers"
  dir.create(jdbcDriverFolder, showWarnings = FALSE)
  DatabaseConnector::downloadJdbcDrivers("postgresql", pathToDriver = jdbcDriverFolder)

  if (!dbms %in% c("postgresql", "sqlite")) {
    DatabaseConnector::downloadJdbcDrivers(dbms, pathToDriver = jdbcDriverFolder)
  }

  withr::defer(
    {
      unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
}

tempDir <- tempfile()
tempDir <- gsub("\\\\", "/", tempDir) # Correct windows path
renvCachePath <- file.path(tempDir, "renv")
withr::defer(
  {
    unlink(tempDir, recursive = TRUE, force = TRUE)
    unlink(renvCachePath, recursive = TRUE, force = TRUE)
  },
  testthat::teardown_env()
)

if (dbms == "sqlite") {
  eunomiaDbFile <- file.path(tempDir, "data", "testEunomia.sqlite")
  if (!dir.exists(file.path(tempDir, "data"))) {
    dir.create(file.path(tempDir, "data"), recursive = T, showWarnings = F)
  }
  connectionDetails <- Eunomia::getEunomiaConnectionDetails(
    databaseFile = eunomiaDbFile
  )
  withr::defer(
    {
      unlink(eunomiaDbFile, recursive = TRUE, force = TRUE)
    },
    testthat::teardown_env()
  )
  cdmDatabaseSchema <- "main"
  workDatabaseSchema <- "main"
  vocabularyDatabaseSchema <- workDatabaseSchema
  cohortTable <- "cohort"
  tempEmulationSchema <- NULL
} else {
  if (dbms == "postgresql") {
    dbUser <- Sys.getenv("CDM5_POSTGRESQL_USER")
    dbPassword <- Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
    dbServer <- Sys.getenv("CDM5_POSTGRESQL_SERVER")
    cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
    vocabularyDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
    tempEmulationSchema <- NULL
    workDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
  } else if (dbms == "oracle") {
    dbUser <- Sys.getenv("CDM5_ORACLE_USER")
    dbPassword <- Sys.getenv("CDM5_ORACLE_PASSWORD")
    dbServer <- Sys.getenv("CDM5_ORACLE_SERVER")
    cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
    vocabularyDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
    tempEmulationSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
    workDatabaseSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
    options(sqlRenderTempEmulationSchema = tempEmulationSchema)
  } else if (dbms == "redshift") {
    dbUser <- Sys.getenv("CDM5_REDSHIFT_USER")
    dbPassword <- Sys.getenv("CDM5_REDSHIFT_PASSWORD")
    dbServer <- Sys.getenv("CDM5_REDSHIFT_SERVER")
    cdmDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
    vocabularyDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
    tempEmulationSchema <- NULL
    workDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
  } else if (dbms == "sql server") {
    dbUser <- Sys.getenv("CDM5_SQL_SERVER_USER")
    dbPassword <- Sys.getenv("CDM5_SQL_SERVER_PASSWORD")
    dbServer <- Sys.getenv("CDM5_SQL_SERVER_SERVER")
    cdmDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
    vocabularyDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA")
    tempEmulationSchema <- NULL
    workDatabaseSchema <- Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA")
  }

  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = dbms,
    user = dbUser,
    password = URLdecode(dbPassword),
    server = dbServer,
    pathToDriver = jdbcDriverFolder
  )
}
