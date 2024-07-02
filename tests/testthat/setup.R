library(testthat)
library(Strategus)
library(Eunomia)
library(dplyr)

if (dir.exists(Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"))) {
  jdbcDriverFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
} else {
  jdbcDriverFolder <- "~/jdbcDrivers"
  dir.create(jdbcDriverFolder, showWarnings = FALSE)
  baseDatabaseConnectorJarFolder <- Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
  Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = jdbcDriverFolder)
  withr::defer(
    {
      unlink(jdbcDriverFolder, recursive = TRUE, force = TRUE)
      Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = baseDatabaseConnectorJarFolder)
    },
    testthat::teardown_env()
  )
}

# Create a unique ID for the table identifiers
tableSuffix <- paste0(substr(.Platform$OS.type, 1, 3), format(Sys.time(), "%y%m%d%H%M%S"), sample(1:100, 1))
tableSuffix <- abs(digest::digest2int(tableSuffix))

tempDir <- tempfile()
tempDir <- gsub("\\\\", "/", tempDir) # Correct windows path

# Assemble a list of connectionDetails for the tests -----------
connectionDetailsList <- list()

# SQLite
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
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = paste0("s", tableSuffix))
tempEmulationSchema <- NULL

connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  workDatabaseSchema = "main",
  vocabularyDatabaseSchema = "main",
  cohortTableNames = cohortTableNames,
  tempEmulationSchema = NULL
)

# PostgreSQL
if (!(Sys.getenv("CDM5_POSTGRESQL_USER") == "" &
  Sys.getenv("CDM5_POSTGRESQL_PASSWORD") == "" &
  Sys.getenv("CDM5_POSTGRESQL_SERVER") == "" &
  Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA") == "" &
  Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA") == "")) {
  DatabaseConnector::downloadJdbcDrivers("postgresql")
  connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
    connectionDetails = DatabaseConnector::createConnectionDetails(
      dbms = "postgresql",
      user = Sys.getenv("CDM5_POSTGRESQL_USER"),
      password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
      server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
      port = 5432,
      pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
    ),
    cdmDatabaseSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    workDatabaseSchema = Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA"),
    vocabularyDatabaseSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    cohortTableNames = cohortTableNames,
    tempEmulationSchema = NULL
  )
}

# Oracle
if (!(Sys.getenv("CDM5_ORACLE_USER") == "" &
  Sys.getenv("CDM5_ORACLE_PASSWORD") == "" &
  Sys.getenv("CDM5_ORACLE_SERVER") == "" &
  Sys.getenv("CDM5_ORACLE_CDM_SCHEMA") == "" &
  Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA") == "")) {
  DatabaseConnector::downloadJdbcDrivers("oracle")
  connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
    connectionDetails = DatabaseConnector::createConnectionDetails(
      dbms = "oracle",
      user = Sys.getenv("CDM5_ORACLE_USER"),
      password = URLdecode(Sys.getenv("CDM5_ORACLE_PASSWORD")),
      server = Sys.getenv("CDM5_ORACLE_SERVER"),
      port = 1521,
      pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
    ),
    cdmDatabaseSchema = Sys.getenv("CDM5_ORACLE_CDM_SCHEMA"),
    workDatabaseSchema = Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA"),
    vocabularyDatabaseSchema = Sys.getenv("CDM5_ORACLE_CDM_SCHEMA"),
    cohortTableNames = cohortTableNames,
    tempEmulationSchema = Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
  )
}

# RedShift
if (!(Sys.getenv("CDM5_REDSHIFT_USER") == "" &
  Sys.getenv("CDM5_REDSHIFT_PASSWORD") == "" &
  Sys.getenv("CDM5_REDSHIFT_SERVER") == "" &
  Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA") == "" &
  Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA") == "")) {
  DatabaseConnector::downloadJdbcDrivers("redshift")
  connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
    connectionDetails = DatabaseConnector::createConnectionDetails(
      dbms = "redshift",
      user = Sys.getenv("CDM5_REDSHIFT_USER"),
      password = URLdecode(Sys.getenv("CDM5_REDSHIFT_PASSWORD")),
      server = Sys.getenv("CDM5_REDSHIFT_SERVER"),
      port = 5439,
      pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
    ),
    cdmDatabaseSchema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
    workDatabaseSchema = Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA"),
    vocabularyDatabaseSchema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
    cohortTableNames = cohortTableNames,
    tempEmulationSchema = NULL
  )
}

# SQL Server
if (!(Sys.getenv("CDM5_SQL_SERVER_USER") == "" &
  Sys.getenv("CDM5_SQL_SERVER_PASSWORD") == "" &
  Sys.getenv("CDM5_SQL_SERVER_SERVER") == "" &
  Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA") == "" &
  Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA") == "")) {
  DatabaseConnector::downloadJdbcDrivers("sql server")
  connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
    connectionDetails = DatabaseConnector::createConnectionDetails(
      dbms = "sql server",
      user = Sys.getenv("CDM5_SQL_SERVER_USER"),
      password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
      server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
      port = 1433,
      pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
    ),
    cdmDatabaseSchema = Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"),
    workDatabaseSchema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA"),
    vocabularyDatabaseSchema = Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"),
    cohortTableNames = cohortTableNames,
    tempEmulationSchema = NULL
  )
}

