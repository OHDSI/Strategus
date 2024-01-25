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

# Update the module version information based on updates found on GitHub
library(dplyr)
# httr::set_config(httr::config(ssl_verifypeer = FALSE))
updateModuleVersionInfo <- function() {
  modules <- CohortGenerator::readCsv(file = "inst/csv/modules.csv")
  modules <- modules %>%
    select(-c("mainPackage", "mainPackageTag"))
  # Get latest module versions ---------------------------------------------------
  getLatestModuleVersion <- function(remoteRepo, remoteUsername, module) {
    urlTemplate <- "https://api.%s/repos/%s/%s/releases/latest"
    release <- jsonlite::fromJSON(sprintf(urlTemplate, remoteRepo, remoteUsername, module))
    return(release$tag_name)
  }
  versions <- tibble::tibble(
    module = modules$module,
    moduleVersion = mapply(getLatestModuleVersion, modules$remoteRepo, modules$remoteUsername, modules$module),
    mainPackage = "",
    mainPackageTag = ""
  )
  # Get referenced main package tag ----------------------------------------------
  for (i in 1:nrow(modules)) {
    module <- versions$module[i]
    if (module == "CohortIncidenceModule") {
      urlTemplate <-   "https://raw.githubusercontent.com/OHDSI/%s/master/renv.lock"
    } else {
      urlTemplate <-   "https://raw.githubusercontent.com/OHDSI/%s/main/renv.lock"
    }
    lock <- jsonlite::fromJSON(sprintf(urlTemplate, module))
    mainPackage <- gsub("Module", "", module)
    versions$mainPackage[i] <- mainPackage
    for (j in seq_along(lock$Packages)) {
      if (lock$Packages[[j]]$Package == mainPackage) {
        if (is.null(lock$Packages[[j]]$RemoteRef) || tolower(lock$Packages[[j]]$RemoteRef) == "head") {
          versions$mainPackageTag[i] <- paste0("v", lock$Packages[[j]]$Version)
        } else {
          versions$mainPackageTag[i] <- lock$Packages[[j]]$RemoteRef
        }
        break
      }
    }
  }
  moduleList <- versions %>%
    dplyr::inner_join(modules, by = c('module' = 'module')) %>%
    dplyr::mutate(version = moduleVersion) %>%
    dplyr::select(c(names(modules), "mainPackage", "mainPackageTag"))

  CohortGenerator::writeCsv(x = moduleList,
                            file = "inst/csv/modules.csv")
}
updateModuleVersionInfo()

# Create manual and vignettes:
unlink("extras/Strategus.pdf")
shell("R CMD Rd2pdf ./ --output=extras/Strategus.pdf")

dir.create("inst/doc")
rmarkdown::render("vignettes/CreatingAnalysisSpecification.Rmd",
                  output_file = "../inst/doc/CreatingAnalysisSpecification.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/CreatingAnalysisSpecification.tex")

rmarkdown::render("vignettes/CreatingModules.Rmd",
                  output_file = "../inst/doc/CreatingModules.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/CreatingModules.tex")

rmarkdown::render("vignettes/ExecuteStrategus.Rmd",
                  output_file = "../inst/doc/ExecuteStrategus.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/ExecuteStrategus.tex")

rmarkdown::render("vignettes/IntroductionToStrategus.Rmd",
                  output_file = "../inst/doc/IntroductionToStrategus.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))
unlink("inst/doc/IntroductionToStrategus.tex")

pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Repackage the test module for unit testing
# NOTE: This is only necessary when the TestModule
# has been updated
testModuleRootFolder <- "extras/TestModule1-0.0.1"
targetModuleZipFile <- "TestModule1_0.0.1.zip"
testModuleFilesToRemove <- c(
  file.path(testModuleRootFolder, ".RData"),
  file.path(testModuleRootFolder, ".Rhistory")
)
testModuleDirToRemove <- c(
  file.path(testModuleRootFolder, ".Rproj.user"),
  file.path(testModuleRootFolder, "renv/library"),
  file.path(testModuleRootFolder, "renv/profiles/dev/renv/library")
)
unlink(testModuleFilesToRemove)
unlink(testModuleDirToRemove, recursive = TRUE)

oldwd <- getwd()
setwd("extras")
zip::zip(
  zipfile = targetModuleZipFile,
  files = list.files("TestModule1-0.0.1", all.files = TRUE, recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
)
file.copy(
  from = targetModuleZipFile,
  to = file.path("../inst/testdata", targetModuleZipFile),
  overwrite = TRUE
)
file.remove(targetModuleZipFile)
setwd(oldwd)
