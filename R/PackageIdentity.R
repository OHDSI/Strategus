# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of Strategus
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0

.getHadesModuleRegistry <- function() {
  CohortGenerator::readCsv(
    file = system.file(
      file.path("csv", "hadesModuleList.csv"),
      package = "Strategus",
      mustWork = TRUE
    ),
    warnOnCaseMismatch = FALSE
  )
}

.getModulePackageName <- function(moduleName) {
  moduleRegistry <- .getHadesModuleRegistry()
  index <- match(tolower(moduleName), tolower(moduleRegistry$module))
  if (is.na(index)) {
    stop("No underlying package is registered for module: ", moduleName, call. = FALSE)
  }
  moduleRegistry$package[[index]]
}

.getInstalledPackageDescription <- function(packageName) {
  if (!requireNamespace(packageName, quietly = TRUE)) {
    stop("Package '", packageName, "' must be installed to record package identity.", call. = FALSE)
  }

  description <- tryCatch(
    as.list(utils::packageDescription(packageName)),
    error = function(error) {
      stop(
        "Could not read DESCRIPTION for installed package '", packageName, "': ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  description
}

.getInstalledPackageVersion <- function(packageName) {
  description <- .getInstalledPackageDescription(packageName)
  version <- description[["Version"]]
  if (is.null(version) || !nzchar(version)) {
    stop("Installed package '", packageName, "' has no Version in DESCRIPTION.", call. = FALSE)
  }
  unname(as.character(version))
}

.getStrategusVersion <- function() {
  .getInstalledPackageVersion("Strategus")
}

.isCranLikePackageRecord <- function(record) {
  remoteType <- record[["RemoteType"]]
  is.null(remoteType) || tolower(remoteType) %in% c("cran", "repository", "standard")
}

.getPackageHashFields <- function(record) {
  fields <- c(
    "Package", "Version", "Title", "Author", "Maintainer", "Description",
    "Depends", "Imports", "Suggests", "LinkingTo"
  )

  if (.isCranLikePackageRecord(record)) {
    remoteSha <- record[["RemoteSha"]]
    if (is.null(remoteSha) || nchar(remoteSha) < 40L) {
      return(fields)
    }
  }

  remoteFields <- grep("^Remote", names(record), perl = TRUE, value = TRUE)
  if (identical(record[["RemoteRef"]], "HEAD")) {
    remoteFields <- setdiff(remoteFields, "RemoteRef")
  }
  c(fields, remoteFields)
}

.computePackageRecordHash <- function(record) {
  if (!is.list(record) || is.null(names(record))) {
    stop("Package record must be a named list.", call. = FALSE)
  }

  dependencyFields <- c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")
  for (field in dependencyFields) {
    if (!is.null(record[[field]])) {
      record[[field]] <- paste(unlist(record[[field]]), collapse = ", ")
    }
  }

  fields <- intersect(.getPackageHashFields(record), names(record))
  selected <- record[fields]
  selected <- selected[order(names(selected), method = "radix")]
  contents <- paste(names(selected), selected, sep = ": ", collapse = "\n")
  contents <- gsub("[[:space:]]", "", contents)

  hashFile <- tempfile("strategus-package-hash-")
  on.exit(unlink(hashFile), add = TRUE)
  writeLines(contents, con = hashFile, useBytes = TRUE)
  unname(as.character(tools::md5sum(hashFile)))
}

.getInstalledPackageHash <- function(packageName) {
  .computePackageRecordHash(.getInstalledPackageDescription(packageName))
}

.getPackageIdentity <- function(packageName) {
  description <- .getInstalledPackageDescription(packageName)
  version <- description[["Version"]]
  if (is.null(version) || !nzchar(version)) {
    stop("Installed package '", packageName, "' has no Version in DESCRIPTION.", call. = FALSE)
  }

  list(
    package = packageName,
    version = unname(as.character(version)),
    hash = .computePackageRecordHash(description)
  )
}
