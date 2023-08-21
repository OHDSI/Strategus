library(testthat)
library(Eunomia)
connectionDetails <- getEunomiaConnectionDetails()

workFolder <- tempfile("work")
dir.create(workFolder)
resultsfolder <- tempfile("results")
dir.create(resultsfolder)
jobContext <- readRDS("tests/testJobContext.rds")
jobContext$moduleExecutionSettings$workSubFolder <- workFolder
jobContext$moduleExecutionSettings$resultsSubFolder <- resultsfolder
jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails

test_that("Run module", {
  source("Main.R")
  execute(jobContext)
  resultsFiles <- list.files(resultsfolder)
  expect_true("tm1_data.csv" %in% resultsFiles)
})

unlink(workFolder)
unlink(resultsfolder)
unlink(connectionDetails$server())
