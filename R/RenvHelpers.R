#' Compare two renv.lock files
#'
#' @description
#' Used to compare renv.lock files and return the results in a data.frame.
#' The return value will include a "full join" representation of the packages
#' across the two lock files.
#'
#' @param filename1 The first renv.lock file name
#'
#' @param filename2 The second renv.lock file name
#'
#' @return
#' A data.frame with the comparison of the rev.lock files
#'
#' @export
compareLockFiles <- function(filename1, filename2) {
  # Read the lock files
  lockfile1 <- renv::lockfile_read(
    file = filename1
  )

  lockfile2 <- renv::lockfile_read(
    file = filename2
  )

  # Compare lock files
  lockfile1Packages <- lockFileToDataFrame(lockfile1)
  names(lockfile1Packages) <- paste0("lockfile1", names(lockfile1Packages))
  lockfile2Packages <- lockFileToDataFrame(lockfile2)
  names(lockfile2Packages) <- paste0("lockfile2", names(lockfile2Packages))
  mergedLockFilePackages <- merge(
    x = lockfile1Packages,
    y = lockfile2Packages,
    by.x = "lockfile1Name",
    by.y = "lockfile2Name",
    all = TRUE
  )
  return(mergedLockFilePackages)
}

#' Synchronize renv.lock files and overwrite the target file
#' (read the description)
#'
#' @description
#' Used to synchronize the values from the "source of truth" renv.lock file to
#' the target renv.lock file. Packages are compared (by name) and if the version
#' of the package in the "source of truth" is greater the one found in the
#' target, the target renv.lock file will be updated. This function will
#' automatically update the target file.
#'
#' Version comparison is handled by the `semver` package and since most packages
#' use semantic versioning. When a package does not use semantic versioning,
#' a warning is provided so the user can review.
#'
#' @param sourceOfTruthLockFileName The renv.lock file to use as the source of
#'                                  truth
#'
#' @param targetLockFileName The target renv.lock file that will be synced with
#'                           the source of truth
#'
#' @return
#' A data.frame containing the different packages and their version that
#' were involved in the synchronization process
#'
#' @export
syncLockFile <- function(sourceOfTruthLockFileName, targetLockFileName) {
  findPackageByName <- function(list, packageName) {
    index <- which(sapply(list, function(x) x$Package == packageName))
    return(index)
  }

  # Read the lock files
  sourceOfTruthLockFile <- renv::lockfile_read(
    file = sourceOfTruthLockFileName
  )
  targetLockFile <- renv::lockfile_read(
    file = targetLockFileName
  )

  # Compare the lock files to get the differences in package versions
  comparedLockFiles <- compareLockFiles(
    filename1 = sourceOfTruthLockFileName,
    filename2 = targetLockFileName
  )
  verDiffs <- comparedLockFiles[!is.na(comparedLockFiles$lockfile2Version) &
    comparedLockFiles$lockfile1Version != comparedLockFiles$lockfile2Version, ]
  verDiffs <- verDiffs[!is.na(verDiffs$lockfile1Name), ]

  if (nrow(verDiffs) == 0) {
    rlang::inform("Lock files are already in sync.")
    return(invisible(NULL))
  }

  # Update the target lock file based on the source of truth
  for (i in 1:nrow(verDiffs)) {
    index <- findPackageByName(targetLockFile$Packages, verDiffs[i, ]$lockfile1Name)
    tryCatch(expr = {
      semverPattern <- "^\\d+\\.\\d+\\.\\d+(?:-[0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*)?(?:\\+[0-9A-Za-z-]+)?$"
      sourceOfTruthVersion <- verDiffs[i, ]$lockfile1Version
      targetVersion <- targetLockFile$Packages[[index]]$Version
      if (grepl(semverPattern, sourceOfTruthVersion) && grepl(semverPattern, targetVersion)) {
        sourceOfTruthVersion <- semver::parse_version(sourceOfTruthVersion)
        targetVersion <- semver::parse_version(targetVersion)
        if (sourceOfTruthVersion > targetVersion) {
          rlang::inform(
            message = paste(verDiffs[i, ]$lockfile1Name, "[", targetVersion, "->", sourceOfTruthVersion, "]")
          )
          targetLockFile$Packages[[index]]$Version <- verDiffs[i, ]$lockfile1Version
          if (!is.na(verDiffs[i, ]$lockfile1RemoteRef)) {
            targetLockFile$Packages[[index]]$RemoteRef <- verDiffs[i, ]$lockfile1RemoteRef
          }
        } else {
          rlang::inform(
            message = paste(verDiffs[i, ]$lockfile1Name, "[ SKIPPING - ", targetVersion, ">", sourceOfTruthVersion, "]")
          )
        }
      } else {
        rlang::warn(paste0("Package: [", verDiffs[i, ]$lockfile1Name, "] - version number could not be parsed. Please inspect manually as it may require an upgrade."))
      }
    }, error = function(err) {
      rlang::inform("An error occurred:", utils::str(err), "\n")
    })
  }

  # Save the updated lock file
  renv::lockfile_write(
    lockfile = targetLockFile,
    file = targetLockFileName
  )

  return(invisible(verDiffs))
}

#' Validate an renv.lock file to ensure it is ready for use by Strategus
#'
#' @description
#' Will check an renv.lock file for a module to verify that it only references
#' tagged packages and includes the packages required by Strategus. It will
#' also check for suggested packages that are useful for testing, such as
#' RSQLite.
#'
#' @param filename The renv.lock file to validate
#'
#' @export
validateLockFile <- function(filename) {
  # Read the lock file
  lockFile <- renv::lockfile_read(
    file = filename
  )
  # Create a data.frame from the renv.lock file
  df <- lockFileToDataFrame(
    lf = lockFile
  )

  # Check that the mandatory dependencies are met
  message("Checking mandatory packages...", appendLF = F)
  if (length(mandatoryPackages()) != nrow(df[df$Name %in% mandatoryPackages(), ])) {
    missingPkgs <- setdiff(mandatoryPackages(), df[df$Name %in% mandatoryPackages(), ]$Name)
    message("FAILED!")
    message(" -- Missing the mandatory packages: ", paste(missingPkgs, collapse = ", "))
    message(" Please record these missing dependencies in your renv.lock file.")
  } else {
    message("PASSED!")
  }

  # Check for suggested packages
  message("Checking suggested packages...", appendLF = F)
  if (length(suggestedPacakges()) != nrow(df[df$Name %in% suggestedPacakges(), ])) {
    missingPkgs <- setdiff(suggestedPacakges(), df[df$Name %in% suggestedPacakges(), ]$Name)
    message("WARNING!")
    message(" -- Missing the suggested packages: ", paste(missingPkgs, collapse = ", "))
    message(" This is an optional set of dependencies so you may decide if you wish to have them in your renv.lock file.")
  } else {
    message("PASSED!")
  }

  # Check that we're using declared versions of all packages
  message("Checking all package using tagged versions in RemoteRef...", appendLF = F)
  # Start by filtering out the CRAN Repository entries
  dfFiltered <- df[tolower(df$Source) != "repository", ]
  if (!all(grepl("^(v)?\\d+(\\.\\d+){2}$", dfFiltered$RemoteRef))) {
    message("FAILED! Please check the following packages:")
    problemPkgs <- dfFiltered[!grepl("^(v)?\\d+(\\.\\d+){2}$", dfFiltered$RemoteRef), ]
    for (i in 1:nrow(problemPkgs)) {
      message(paste0(" -- Package: ", problemPkgs$Name[[i]], "; RemoteRef: ", problemPkgs$RemoteRef[[i]]))
    }
    message(" Please ensure you are only including tagged versions of package dependencies in your renv.lock file.")
  } else {
    message("PASSED!")
  }
}

#' List of mandatory packages for a Strategus module
#'
#' @keywords internal
mandatoryPackages <- function() {
  return(c(
    "CohortGenerator",
    "DatabaseConnector",
    "keyring",
    "ParallelLogger",
    "renv",
    "SqlRender"
  ))
}

#' List of suggested packages for a Strategus module
#'
#' @keywords internal
suggestedPacakges <- function() {
  return(c("RSQLite"))
}


#' Convert a lock file to a data.frame
#'
#' @keywords internal
lockFileToDataFrame <- function(lf) {
  df <- data.frame()
  for (i in 1:length(lf$Packages)) {
    df <- rbind(
      df,
      data.frame(
        Name = lf$Packages[[i]]$Package,
        Version = lf$Packages[[i]]$Version,
        Source = lf$Packages[[i]]$Source,
        RemoteRef = ifelse(is.null(lf$Packages[[i]]$RemoteRef), yes = NA, no = lf$Packages[[i]]$RemoteRef)
      )
    )
  }
  return(df)
}
