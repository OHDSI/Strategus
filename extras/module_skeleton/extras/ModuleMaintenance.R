# Format and check code:
styler::style_dir()
OhdsiRTools::updateCopyrightYearFolder()
OhdsiRTools::findNonAsciiStringsInFolder()
devtools::spell_check()

# Generate renv lock file and activate renv:
OhdsiRTools::createRenvLockFile(
  rootPackage = "CohortGenerator",
  includeRootPackage = TRUE,
  mode = "description",
  additionalRequiredPackages = c("checkmate", "CirceR")
)
renv::init()
