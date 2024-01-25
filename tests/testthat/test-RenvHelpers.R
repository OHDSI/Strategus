test_that("Check renv.lock compare", {
  compare <- Strategus::compareLockFiles(
    filename1 = system.file("testdata/renv.lock", package = "Strategus"),
    filename2 = system.file("testdata/renv.lock", package = "Strategus")
  )
  expect_true(nrow(compare) > 0)
  expect_true(nrow(compare[compare$lockfile1Version != compare$lockfile1Version,]) == 0)
})

test_that("Check renv.lock sync detects no changes", {
  expect_null(
    Strategus::syncLockFile(
      sourceOfTruthLockFileName = system.file("testdata/renv.lock", package = "Strategus"),
      targetLockFileName = system.file("testdata/renv.lock", package = "Strategus")
    )
  )
})

test_that("Check renv.lock sync works", {
  tempDir <- tempdir()
  tempLockFile <- file.path(tempDir, "renv.lock")
  on.exit(unlink(tempLockFile))
  file.copy(
    from = system.file("testdata/renv.lock", package = "Strategus"),
    to = file.path(tempDir, "renv.lock")
  )
  # Get the expected value
  lf <- renv::lockfile_read(
    file = system.file("testdata/renv.lock", package = "Strategus")
  )
  expectedVersion <- lf$Packages$zip$Version
  oldVersion <- "0.0.1"
  renv::record(
    records = paste0("zip@", oldVersion),
    lockfile = tempLockFile
  )

  # Confirm the update ahead of the test worked
  tempLf <- renv::lockfile_read(
    file = tempLockFile
  )
  expect_equal(tempLf$Packages$zip$Version, oldVersion)

  # Perform the sync
  Strategus::syncLockFile(
    sourceOfTruthLockFileName = system.file("testdata/renv.lock", package = "Strategus"),
    targetLockFileName = tempLockFile
  )

  # Confirm that the newer version from the source of truth was applied
  tempLf <- renv::lockfile_read(
    file = tempLockFile
  )
  expect_equal(tempLf$Packages$zip$Version, expectedVersion)
})
