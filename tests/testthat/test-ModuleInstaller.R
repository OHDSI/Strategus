test_that("Modules install from github", {
  installedModulesPath <- tempfile()
  unlink(installedModulesPath, recursive = TRUE)
  on.exit(unlink(installedModulesPath, recursive = TRUE))

  installGithubModule("OHDSI/CohortGeneratorModule", installedModulesPath = installedModulesPath)
  checkmate::expect_directory_exists(file.path(installedModulesPath, "/CohortGeneratorModule"))
  expect_error(installGithubModule("OHDSI/CohortGeneratorModule", installedModulesPath = installedModulesPath))

  checkmate::expect_data_frame(getAvailableModules(installedModulesPath))

  installGithubModule("OHDSI/CohortGeneratorModule", installedModulesPath = installedModulesPath, overwrite = TRUE)
  installGithubModule("OHDSI/CohortMethodModule", installedModulesPath = installedModulesPath)
  checkmate::expect_directory_exists(file.path(installedModulesPath, "CohortGeneratorModule"))

  # Should not be able to install if lock file is present
  installLockFile <- file.path(installedModulesPath, "install_module.lock")
  writeLines(timestamp(quiet = TRUE), con = installLockFile)

  expect_error(installGithubModule("OHDSI/CohortGeneratorModule", installedModulesPath = installedModulesPath, overwrite = TRUE))
  unlink(installLockFile)
})