rootDir <- "C:/git/OHDSI"
moduleList <- c(
  "Characterization",
  "CohortDiagnostics",
  "CohortGenerator",
  "CohortIncidence",
  "CohortMethod",
  "EvidenceSynthesis",
  "PatientLevelPrediction",
  "SelfControlledCaseSeries"
)

for (i in seq_along(moduleList)) {
  repoPath <- file.path(rootDir, paste0(moduleList[i], "Module"))
  if (dir.exists(repoPath)) {
    cat("Checking ", repoPath, "\n")
    cat("  -- Checking renv.lock file\n")
    Strategus::validateLockFile(filename = file.path(repoPath, "renv.lock"))
    cat("  -- Checking dev/renv.lock file\n")
    Strategus::validateLockFile(filename = file.path(repoPath, "renv/profiles/dev/renv.lock"))
  } else {
    warning(paste0(repoPath, "NOT FOUND!!"))
  }
}
