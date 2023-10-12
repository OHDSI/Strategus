# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of Strategus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Note: Using S3 for consistency with settings objects in PLP, CohortMethod, and
# FeatureExtraction. If we want to use S4 or R6 we need to first adapt those
# packages. This will be difficult, since these settings objects are used throughout
# these packages, and are for example used in do.call() calls. We should also
# carefully consider serialization and deserialization to JSON, which currently
# uses custom functionality in ParallelLogger to maintain object attributes.

# Install keyring - one time operation ---------
appendToRenviron <- function(varName, value, environFile = "~/.Renviron") {
  if (file.exists(environFile))
    lines <- readLines(environFile)
  else
    lines <- c()

  if (any(grepl(varName, lines))) {
    cli::cli_alert_info("Found existing environment variable {varName}, last value set will be taken by system")
  }

  renviron <- c(lines, glue::glue("{varName}='{value}'"))
  writeLines(renviron, environFile)
}

#' Create Strategus Keyring
#' @description
#' CLI utility for creating a strategus keyring.
#'
#' This will perform the required setup for secure credentials inside a study.
#'
#' Creates a system wide keyring password if not already present.
#' Adds connectionDetails to it.
#' @export
#' @param keyringName                   string name for keyring on system to store credentials inside of
#' @param connectionDetailsReference    string reference name for connection details e.g. "defautCdm". This will be used
#'                                      in strategus executions to reference your database. Connection details stored for this
#'                                      will be overriden if they already exist.
#' @param connectionDetails             (optional) connectionDetails object to store in keyring. If not set user will be
#'                                      prompted for one.
createStrategusKeyring <- function(keyringName,
                                   connectionDetailsReference = "defautCdm",
                                   connectionDetails = NULL) {
  if (!interactive()) {
    stop("Requires interactive session")
  }

  checkmate::assertString(keyringName)
  checkmate::assertString(connectionDetailsReference)
  checkmate::assertClass(connectionDetails, "ConnectionDetails", null.ok = TRUE)

  # These packages are optional for this wizard
  requiredPackages <- c("keyring", "cli", "getPass")
  requiredPackages <- requiredPackages[!requiredPackages %in% as.data.frame(installed.packages())$Package]

  if (length(requiredPackages)) {
    resp <- utils::menu(c("yes", "no"), title = paste("Package", requiredPackages, "not installed. install now?"))
    if (resp != 1) {
      stop("Required system packages missing for this configuration utility")
    }
  }

  cli::cli_h1("Strategus Keying Setup")
  cli::cli_text("Creating keyring {keyringName}")
  cli::cli_text("This script is designed to set up your variables for your strategus keyring.")
  cli::cli_text("You will be prompted for a number of inputs that will be validated.")

  cli::cli_h2("Setting system environment variables")
  if (Sys.getenv("STRATEGUS_KEYRING_PASSWORD") == "") {
    cli::cli_par("Please enter a password for your keyring")
    passVar <- getPass::getPass("Keyring password", noblank = TRUE)

    if (passVar == "" || is.null(passVar)) {
      cli::cli_abort("Must enter a password")
    }
    Sys.setenv("STRATEGUS_KEYRING_PASSWORD" = passVar)
    appendToRenviron("STRATEGUS_KEYRING_PASSWORD", passVar)
  }

  cli::cli_alert_success("STRATEGUS_KEYRING_PASSWORD environment var set")

  strategusModuleFolder <- Sys.getenv("INSTANTIATED_MODULES_FOLDER")
  if (strategusModuleFolder == "") {
    while (!checkmate::test_directory(strategusModuleFolder, "w")) {
      cli::cli_alert_info("INSTANTIATED_MODULES_FOLDER environment variable not set please enter a directory:")
      strategusModuleFolder <- readLines(n=1)
      dir.create(strategusModuleFolder, showWarnings = FALSE)
    }
    Sys.setenv("INSTANTIATED_MODULES_FOLDER" = strategusModuleFolder)
    appendToRenviron("INSTANTIATED_MODULES_FOLDER", strategusModuleFolder)
  }

  cli::cli_alert_success("INSTANTIATED_MODULES_FOLDER environment var set")

  cli::cli_h2("Setting database connection")
  tempconnectionFile <- tempfile(fileext = ".R")

  cli::cli_alert_info("Creating file {tempconnectionFile} for Database credentials - this file will be automaticaly removed when completed and the settings saved securly")
  on.exit(unlink(tempconnectionFile, force = TRUE))

  ## These lines will be converted to a string. Substitue is used for readability only
  rscript <- substitute({
# Enter your database connection values here and press save
# the connection will be tested
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = '',
    server = '',
    password = '',
    user = '',
    port = 0,
    extraSettings = NULL,
    connectionString = NULL,
    pathToDriver = Sys.getenv('DATABASECONNECTOR_JAR_FOLDER')
  )
  })
  rscript <- as.character(rscript)[-1]
  writeLines(rscript, con = tempconnectionFile)
  connectionValid <- FALSE

  if (!is.null(connectionDetails)) {
     tryCatch(
    {
      source(tempconnectionFile)
      conn <- DatabaseConnector::connect(connectionDetails)
      DatabaseConnector::disconnect(conn)
      connectionValid <- TRUE
      cli::cli_alert_success("Database Connection Works")
    },
      error = function(message) {
        cli::cli_alert_warning("Database Connection Failed, will require setting manually")
        cli::cli_alert(message)
      }
    )
  }

  while (!connectionValid) {
    resp <- utils::menu(c("yes", "no"), title = "Secure creation of connection details required, continue?")
    if (resp != 1) {
      cli::cli_abort("Secure database credentials cannot be aquired")
    }
    utils::file.edit(tempconnectionFile)
    # test the connection
    tryCatch(
    {
      source(tempconnectionFile)
      conn <- DatabaseConnector::connect(connectionDetails)
      DatabaseConnector::disconnect(conn)
      connectionValid <- TRUE
      cli::cli_alert_success("Database Connection Works")
    },
      error = function(message) {
        cli::cli_alert_warning("Database Connection Failed, retrying...")
        cli::cli_alert(message)
      }
    )
  }
  unlink(tempconnectionFile, force = TRUE)
  cli::cli_alert_info("Removed temporary credentials file {tempconnectionFile}")

  cli::cli_h2("Storing values to secure keyring")
  keyringPassword <- Sys.getenv("STRATEGUS_KEYRING_PASSWORD") # This password is simply to avoid a prompt when creating the keyring
  # Create the keyring if it does not exist.
  # If it exists, clear it out so we can re-load the keys
  allKeyrings <- keyring::keyring_list()
  if (keyringName %in% allKeyrings$keyring) {
    if (keyring::keyring_is_locked(keyring = keyringName)) {
      tryCatch(
      {
        keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
      }, function(error) {
        cli::cli_alert_warning("STRATEGUS_KEYRING_PASSWORD value did not unlock keyring.")
        cli::cli_alert(message)
      })
    }
  } else {
    keyring::keyring_create(keyring = keyringName, password = keyringPassword)
  }
  # excecute this for each connectionDetails/ConnectionDetailsReference you are going to use
  storeConnectionDetails(
    connectionDetails = connectionDetails,
    connectionDetailsReference = connectionDetailsReference,
    keyringName = keyringName
  )
  cli::cli_alert_success("Secure strategus keyring reference {keyringName} created")
  cli::cli_rule()
}
