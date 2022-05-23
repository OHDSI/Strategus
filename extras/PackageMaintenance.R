# Copyright 2022 Observational Health Data Sciences and Informatics
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

# Manually delete package from library. Avoids "Already in use" message when rebuilding
unloadNamespace("Strategus")
.rs.restartR()
folder <- system.file(package = "Strategus")
folder
unlink(folder, recursive = TRUE, force = TRUE)
file.exists(folder)

# Format and check code:
styler::style_pkg()
OhdsiRTools::checkUsagePackage("Strategus")
OhdsiRTools::updateCopyrightYearFolder()
OhdsiRTools::findNonAsciiStringsInFolder()
devtools::spell_check()

# Create manual and vignettes:
unlink("extras/Strategus.pdf")
shell("R CMD Rd2pdf ./ --output=extras/Strategus.pdf")

pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Embed core modules:
getModule <- function(module, version, remoteRepo, remoteUsername) {
  moduleFolder <- file.path("inst", "modules", sprintf("%s_%s", module, version))
  dir.create(moduleFolder, recursive = TRUE)
  moduleFile <- tempfile()
  moduleUrl <- sprintf("https://%s/%s/%s/archive/refs/tags/v%s.zip", remoteRepo, remoteUsername, module, version)
  utils::download.file(url = moduleUrl, destfile = moduleFile)
  utils::unzip(zipfile = moduleFile, exdir = moduleFolder)
  unlink(moduleFile)
  subFolder <- list.dirs(path = moduleFolder, recursive = FALSE)[1]
  R.utils::copyDirectory(from = subFolder, to = moduleFolder, recursive = TRUE)
  unlink(subFolder, recursive = TRUE)
  unlink(file.path(moduleFolder, c(".Rprofile", "renv.lock", "renv", "extras")), recursive = TRUE)
}

getModule(module = "CohortGeneratorModule",
          version = "0.0.4",
          remoteRepo = "github.com",
          remoteUsername = "anthonysena")
getModule(module = "CohortDiagnosticsModule",
          version = "0.0.1",
          remoteRepo = "github.com",
          remoteUsername = "ohdsi")

# Generate central lock file:
tempFolder <- tempfile("strategus")
dir.create(tempFolder)
R.utils::copyDirectory(from = ".", to = tempFolder, recursive = TRUE)
unlink(file.path(tempFolder, "extras"), recursive = TRUE)
moduleFolders <- list.files(file.path("inst", "modules"), include.dirs = TRUE, full.names = TRUE)

copyRScripts <- function(moduleFolder) {
  generateRandomName <- function(dummy) {
    paste(c('t', sample(c(0:9, LETTERS), 10, replace = TRUE), ".R"), collapse = "")
  }
  sourceFiles <- list.files(moduleFolder, pattern = ".R$", full.names = TRUE)
  targetFiles <- file.path(tempFolder, "R", sapply(sourceFiles, generateRandomName))
  file.copy(from = sourceFiles, to = targetFiles)
}
invisible(lapply(moduleFolders, copyRScripts))

wd <- getwd()
renv::init(project = tempFolder, restart = FALSE)
setwd(wd)

# (Copied from OhdsiRTools. Make exportable if we like this route)
lock <- RJSONIO::fromJSON(file.path(tempFolder, "renv.lock"))
tagExists <- function(repo, tag) {
  url <- sprintf("https://github.com/OHDSI/%s/tree/%s", repo, tag)
  response <- httr::GET(url)
  return(response$status_code != "404")
}

for (i in 1:length(lock$Packages)) {
  if (lock$Packages[[i]]["Source"] == "GitHub") {
    if (!lock$Packages[[i]]["Package"] %in% OhdsiRTools::getOhdsiGitHubPackages()) {
      warning(sprintf("Found GitHub package '%s' that is not an OHDSI GitHub package.", lock$Packages[[i]]["Package"]))
    } else {
      remoteRef <- sprintf("v%s", lock$Packages[[i]]["Version"])
      if (!tagExists(lock$Packages[[i]]["Package"], remoteRef)) {
        warning(sprintf(
          "Tag '%s' does not exist for package '%s'. Did you install a develop version? Please only use released package versions.",
          remoteRef,
          lock$Packages[[i]]["Package"]
        ))
      } else {
        message(sprintf("Settings remote ref for package '%s' to '%s'", lock$Packages[[i]]["Package"], remoteRef))
        toDelete <- which(names(lock$Packages[[i]]) %in% c("RemoteSha", "Hash"))
        lock$Packages[[i]] <- lock$Packages[[i]][-toDelete]
        lock$Packages[[i]]["RemoteRef"] <- remoteRef
      }
    }
  }
}
json <- RJSONIO::toJSON(lock, pretty = TRUE)
sink("renv.lock")
cat(json)
sink()


unlink(tempFolder, recursive = TRUE)
renv::init()

