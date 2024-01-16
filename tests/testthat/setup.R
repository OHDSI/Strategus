library(testthat)
library(Strategus)
library(Eunomia)
library(dplyr)

# allows unit tests to run on mac without issue
baseBackend <- Sys.getenv("R_KEYRING_BACKEND")
Sys.setenv("R_KEYRING_BACKEND" = "file")
withr::defer(
  {
    Sys.setenv("R_KEYRING_BACKEND" = baseBackend)
  },
  testthat::teardown_env()
)

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

usingTempDir <- Sys.getenv("STRATEGUS_UNIT_TEST_FOLDER") == ""
tempDir <- ifelse(usingTempDir, tempfile(), Sys.getenv("STRATEGUS_UNIT_TEST_FOLDER"))
tempDir <- gsub("\\\\", "/", tempDir) # Correct windows path
renvCachePath <- file.path(tempDir, "strategus/renv")
moduleFolder <- file.path(tempDir, "strategus/modules")
Sys.setenv("INSTANTIATED_MODULES_FOLDER" = moduleFolder)
withr::defer(
  {
    if (usingTempDir) {
      unlink(c(tempDir, renvCachePath, moduleFolder), recursive = TRUE, force = TRUE)
    }
  },
  testthat::teardown_env()
)

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
cohortTable <- "cohort"
tempEmulationSchema <- NULL

connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  workDatabaseSchema = "main",
  vocabularyDatabaseSchema = "main",
  cohortTable = "cohort",
  tempEmulationSchema = NULL
)
#
# # PostgreSQL
# if (!(Sys.getenv("CDM5_POSTGRESQL_USER") == "" &
#   Sys.getenv("CDM5_POSTGRESQL_PASSWORD") == "" &
#   Sys.getenv("CDM5_POSTGRESQL_SERVER") == "" &
#   Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA") == "" &
#   Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA") == "")) {
#   DatabaseConnector::downloadJdbcDrivers("postgresql")
#   connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
#     connectionDetails = DatabaseConnector::createConnectionDetails(
#       dbms = "postgresql",
#       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
#       password = URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD")),
#       server = Sys.getenv("CDM5_POSTGRESQL_SERVER"),
#       port = 5432,
#       pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
#     ),
#     cdmDatabaseSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
#     workDatabaseSchema = Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA"),
#     vocabularyDatabaseSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
#     cohortTable = "cohort",
#     tempEmulationSchema = NULL
#   )
# }
#
# # Oracle
# if (!(Sys.getenv("CDM5_ORACLE_USER") == "" &
#   Sys.getenv("CDM5_ORACLE_PASSWORD") == "" &
#   Sys.getenv("CDM5_ORACLE_SERVER") == "" &
#   Sys.getenv("CDM5_ORACLE_CDM_SCHEMA") == "" &
#   Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA") == "")) {
#   DatabaseConnector::downloadJdbcDrivers("oracle")
#   connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
#     connectionDetails = DatabaseConnector::createConnectionDetails(
#       dbms = "oracle",
#       user = Sys.getenv("CDM5_ORACLE_USER"),
#       password = URLdecode(Sys.getenv("CDM5_ORACLE_PASSWORD")),
#       server = Sys.getenv("CDM5_ORACLE_SERVER"),
#       port = 1521,
#       pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
#     ),
#     cdmDatabaseSchema = Sys.getenv("CDM5_ORACLE_CDM_SCHEMA"),
#     workDatabaseSchema = Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA"),
#     vocabularyDatabaseSchema = Sys.getenv("CDM5_ORACLE_CDM_SCHEMA"),
#     cohortTable = "cohort",
#     tempEmulationSchema = Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
#   )
# }
#
# # RedShift
# if (!(Sys.getenv("CDM5_REDSHIFT_USER") == "" &
#   Sys.getenv("CDM5_REDSHIFT_PASSWORD") == "" &
#   Sys.getenv("CDM5_REDSHIFT_SERVER") == "" &
#   Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA") == "" &
#   Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA") == "")) {
#   DatabaseConnector::downloadJdbcDrivers("redshift")
#   connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
#     connectionDetails = DatabaseConnector::createConnectionDetails(
#       dbms = "redshift",
#       user = Sys.getenv("CDM5_REDSHIFT_USER"),
#       password = URLdecode(Sys.getenv("CDM5_REDSHIFT_PASSWORD")),
#       server = Sys.getenv("CDM5_REDSHIFT_SERVER"),
#       port = 5439,
#       pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
#     ),
#     cdmDatabaseSchema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
#     workDatabaseSchema = Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA"),
#     vocabularyDatabaseSchema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
#     cohortTable = "cohort",
#     tempEmulationSchema = NULL
#   )
# }
#
# # SQL Server
# if (!(Sys.getenv("CDM5_SQL_SERVER_USER") == "" &
#   Sys.getenv("CDM5_SQL_SERVER_PASSWORD") == "" &
#   Sys.getenv("CDM5_SQL_SERVER_SERVER") == "" &
#   Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA") == "" &
#   Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA") == "")) {
#   DatabaseConnector::downloadJdbcDrivers("sql server")
#   connectionDetailsList[[length(connectionDetailsList) + 1]] <- list(
#     connectionDetails = DatabaseConnector::createConnectionDetails(
#       dbms = "sql server",
#       user = Sys.getenv("CDM5_SQL_SERVER_USER"),
#       password = URLdecode(Sys.getenv("CDM5_SQL_SERVER_PASSWORD")),
#       server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
#       port = 1433,
#       pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")
#     ),
#     cdmDatabaseSchema = Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"),
#     workDatabaseSchema = Sys.getenv("CDM5_SQL_SERVER_OHDSI_SCHEMA"),
#     vocabularyDatabaseSchema = Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"),
#     cohortTable = "cohort",
#     tempEmulationSchema = NULL
#   )
# }

# Keyring helpers --------------
# Set the keyring name & password for testing
keyringName <- "strategus"
keyringPassword <- "ohdsi"

deleteKeyringForUnitTest <- function(selectedKeyring = keyringName, selectedKeyringPassword = keyringPassword) {
  # Create a keyring called "strategus" that is password protected
  allKeyrings <- keyring::keyring_list()
  if (selectedKeyring %in% allKeyrings$keyring) {
    if (keyring::keyring_is_locked(keyring = selectedKeyring)) {
      keyring::keyring_unlock(keyring = selectedKeyring, password = selectedKeyringPassword)
    }
    # Delete all keys from the keyring so we can delete it
    keys <- keyring::key_list(keyring = selectedKeyring)
    if (nrow(keys) > 0) {
      for (i in 1:nrow(keys)) {
        keyring::key_delete(keys$service[1], keyring = selectedKeyring)
      }
    }
    keyring::keyring_delete(keyring = selectedKeyring)
  }
}

createKeyringForUnitTest <- function(selectedKeyring = keyringName, selectedKeyringPassword = keyringPassword) {
  # Delete any existing keyrings
  deleteKeyringForUnitTest(selectedKeyring = selectedKeyring)
  # Create & Lock the keyring
  keyring::keyring_create(keyring = selectedKeyring, password = selectedKeyringPassword)
  keyring::keyring_lock(keyring = selectedKeyring)
}

skip_if_not_secret_service <- function() {
  if (keyring::default_backend()$name != "secret service") skip("Not secret service")
  invisible(TRUE)
}
