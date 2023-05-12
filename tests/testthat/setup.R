library(testthat)
library(Strategus)
library(Eunomia)
library(dplyr)

# allows unit tests to run on mac without issue
baseBackend <- Sys.getenv("R_KEYRING_BACKEND")
Sys.setenv("R_KEYRING_BACKEND" = "file")

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
     Sys.setenv("R_KEYRING_BACKEND" = baseBackend)
    },
    testthat::teardown_env()
  )
}

# Create a unique ID for the table identifiers
tableSuffix <- paste0(substr(.Platform$OS.type, 1, 3), format(Sys.time(), "%y%m%d%H%M%S"), sample(1:100, 1))
tableSuffix <- abs(digest::digest2int(tableSuffix))

tempDir <- tempfile() # "C:/temp"
tempDir <- gsub("\\\\", "/", tempDir) # Correct windows path
renvCachePath <- file.path(tempDir, "strategus/renv")
moduleFolder <- file.path(tempDir, "strategus/modules")
Sys.setenv("INSTANTIATED_MODULES_FOLDER" = moduleFolder)
withr::defer(
  {
    unlink(c(tempDir, renvCachePath, moduleFolder), recursive = TRUE, force = TRUE)
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
    dbPort <- 5432
    cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
    vocabularyDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
    tempEmulationSchema <- NULL
    workDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_OHDSI_SCHEMA")
  } else if (dbms == "oracle") {
    dbUser <- Sys.getenv("CDM5_ORACLE_USER")
    dbPassword <- Sys.getenv("CDM5_ORACLE_PASSWORD")
    dbServer <- Sys.getenv("CDM5_ORACLE_SERVER")
    dbPort <- 1521
    cdmDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
    vocabularyDatabaseSchema <- Sys.getenv("CDM5_ORACLE_CDM_SCHEMA")
    tempEmulationSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
    workDatabaseSchema <- Sys.getenv("CDM5_ORACLE_OHDSI_SCHEMA")
    options(sqlRenderTempEmulationSchema = tempEmulationSchema)
  } else if (dbms == "redshift") {
    dbUser <- Sys.getenv("CDM5_REDSHIFT_USER")
    dbPassword <- Sys.getenv("CDM5_REDSHIFT_PASSWORD")
    dbServer <- Sys.getenv("CDM5_REDSHIFT_SERVER")
    dbPort <- 5439
    cdmDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
    vocabularyDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
    tempEmulationSchema <- NULL
    workDatabaseSchema <- Sys.getenv("CDM5_REDSHIFT_OHDSI_SCHEMA")
  } else if (dbms == "sql server") {
    dbUser <- Sys.getenv("CDM5_SQL_SERVER_USER")
    dbPassword <- Sys.getenv("CDM5_SQL_SERVER_PASSWORD")
    dbServer <- Sys.getenv("CDM5_SQL_SERVER_SERVER")
    dbPort <- 1433
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
    port = dbPort,
    pathToDriver = jdbcDriverFolder
  )
}


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
