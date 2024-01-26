# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of CohortGeneratorModule
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

# Format and check code:
styler::style_dir()
OhdsiRTools::updateCopyrightYearFolder(path = ".", recursive = F)
OhdsiRTools::updateCopyrightYearFolder(path = "./extras", recursive = F)
OhdsiRTools::findNonAsciiStringsInFolder()

# Generate renv lock file for default & dev profiles ------------
# updatedPackages and updatedDevPackages are those packages that either
#   1. Cannot be synced between the HADES-wide lock file and the project lock files
#      since they are not in semver format
#   2. Updates to HADES packages
# NOTE: Mandatory Strategus dependencies:
#   CohortGenerator
#   DatabaseConnector
#   keyring
#   ParallelLogger
#   SqlRender
# are explicitly included in Main.R as library calls to allow renv's init()
# function to find them and include them even if they are not used in the
# module code.

updatedPackages <- list(
  list(
    Package = "askpass",
    Version = "1.2.0",
    Source = "Repository",
    Repository = "CRAN"
  ),
  "OHDSI/CohortGenerator@v0.8.1",
  "OHDSI/ResultModelManager@v0.5.6"
)
updatedDevPackages <- list(
  list(
    Package = "evaluate",
    Version = "0.22",
    Source = "Repository",
    Repository = "CRAN"
  ),
  "OHDSI/Eunomia@v1.0.2"
)

# Deactivates and cleans the project to remove any/all old references
renv::deactivate(clean = TRUE)

# Initialize the default profile ---------
renv::activate(profile = NULL)
# Use the implicit option so renv crawls the full project.
renv::init(settings = renv::settings$snapshot.type("implicit"))
# Record the explicit package versions mentioned above
renv::record(updatedPackages)
# Force a restore for the default profile
renv::restore(prompt = FALSE)

# Initialize the dev profile ------------
renv::activate(profile = "dev") # Creates a new profile called "dev" for development

# Remove the "tests" directory from the .renvignore
# so the test dependencies are included in the dev lock file
file.copy(".renvignore", ".renvignore-backup", overwrite = TRUE)
ignoreFileContents <- readLines(".renvignore")
ignoreFileContents <- ignoreFileContents[!grepl("/tests/", ignoreFileContents)]
writeLines(ignoreFileContents, ".renvignore")

# Capture the dependencies
renv::init(profile = "dev") # Init the 'dev' profile renv.lock with the explicit DESCRIPTION references

# Record the updated packages
renv::record(c(updatedPackages, updatedDevPackages), lockfile = "renv/profiles/dev/renv.lock")

# Force a restore for the dev profile
renv::restore(prompt = FALSE)

# Restore the original .renvignore
unlink(".renvignore")
file.rename(".renvignore-backup", ".renvignore")

# Re-activate the default profile - the dev profile is only used for unit tests
renv::activate(profile = NULL) # Sets the default profile as the default for the project

# Sync lock files with HADES-wide lock file --------------
hadesWideLockFileName <- normalizePath("hades-wide.lock")
unlink(hadesWideLockFileName)
utils::download.file(
  url = "https://raw.githubusercontent.com/OHDSI/Hades/main/hadesWideReleases/2023Q3/renv.lock",
  destfile = hadesWideLockFileName
)
# Verify the package versions across lock files
compareLockFiles <- function(filename1, filename2) {
  # Read the lock files
  lockfile1 <- renv::lockfile_read(
    file = filename1
  )
  print(normalizePath(filename2))
  lockfile2 <- renv::lockfile_read(
    file = filename2
  )
  # internal function to read lock file into data frame
  lockFileToDataFrame <- function(lf) {
    df <- data.frame()
    for (i in 1:length(lf$Packages)) {
      df <- rbind(
        df,
        data.frame(
          Name = lf$Packages[[i]]$Package,
          Version = lf$Packages[[i]]$Version,
          RemoteRef = ifelse(is.null(lf$Packages[[i]]$RemoteRef), yes = NA, no = lf$Packages[[i]]$RemoteRef)
        )
      )
    }
    return(df)
  }
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

# Compare HADES-wide lock file to the dev lock file
hwVsProjDevLockFile <- compareLockFiles(
  filename1 = hadesWideLockFileName,
  filename2 = "renv/profiles/dev/renv.lock"
)
hwVsProjDevLockFile[!is.na(hwVsProjDevLockFile$lockfile2Version) & hwVsProjDevLockFile$lockfile1Version != hwVsProjDevLockFile$lockfile2Version, ]

# Compare project default lock file to the dev lock file
projDevVsProjLockFile <- compareLockFiles(
  filename1 = "renv/profiles/dev/renv.lock",
  filename2 = "renv.lock"
)
projDevVsProjLockFile[!is.na(projDevVsProjLockFile$lockfile2Version) & projDevVsProjLockFile$lockfile1Version != projDevVsProjLockFile$lockfile2Version, ]

# Given a source of truth lock file, update the target
# lock file. Only replace the version in the target
# lock file if the version is newer. Provide a warning
# for those packages that could not be evaluated by
# version #
renv::install("semver")
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
    # Can we detect if the version is greater
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
      rlang::inform("An error occurred:", str(err), "\n")
    })
  }

  # Save the updated lock file
  renv::lockfile_write(
    lockfile = targetLockFile,
    file = targetLockFileName
  )
}

syncLockFile(
  sourceOfTruthLockFileName = hadesWideLockFileName,
  targetLockFileName = "renv/profiles/dev/renv.lock"
)

syncLockFile(
  sourceOfTruthLockFileName = "renv/profiles/dev/renv.lock",
  targetLockFileName = "renv.lock"
)

# NOTE: Use the compare functions above to verify the files are in sync
# and add any dependencies that could not be synced automatically to the
# updatedPackages and updatedDevPackages respectively.
