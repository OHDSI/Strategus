# library(testthat)
# test_that("Test zip file for sharing results", {
#   testDir <- tempfile()
#   dir.create(testDir)
#   on.exit(unlink(testDir))
#   writeLines(
#     text = "test 1 2 3",
#     file.path(testDir, "log.txt")
#   )
#   dir.create(file.path(testDir, "test"))
#   writeLines(
#     text = "csv",
#     file.path(testDir, "test", "test.csv")
#   )
#
#   zipResults(
#     resultsFolder = testDir,
#     zipFile = file.path(testDir, "test.zip")
#   )
#
#   expect_true(file.exists(file.path(testDir, "test.zip")))
#
#   # Verify that only the csv was zipped
#   zipFiles <- utils::unzip(
#     zipfile = file.path(testDir, "test.zip"),
#     list = TRUE
#   )
#   expect_equal(length(zipFiles$Name), expected = 1)
#   expect_equal(zipFiles$Name[1], file.path("test", "test.csv"))
# })
