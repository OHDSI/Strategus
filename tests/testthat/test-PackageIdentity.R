test_that("package record hashes follow the renv DESCRIPTION algorithm", {
  repositoryRecord <- list(
    Package = "example",
    Version = "1.0.0",
    Title = "An Example",
    Description = "A multiline\n description.",
    Imports = c("R6 (>= 2.0)", "digest"),
    RemoteType = "repository",
    RemoteSha = "short"
  )
  githubRecord <- list(
    Package = "example",
    Version = "1.0.0",
    Title = "An Example",
    RemoteType = "github",
    RemoteHost = "api.github.com",
    RemoteRepo = "example",
    RemoteUsername = "ohdsi",
    RemoteRef = "HEAD",
    RemoteSha = "0123456789012345678901234567890123456789"
  )

  expect_identical(
    Strategus:::.computePackageRecordHash(repositoryRecord),
    "e9953b1ffb833c91723a9cea8165ed67"
  )
  expect_identical(
    Strategus:::.computePackageRecordHash(githubRecord),
    "eb4964261f0c4d0376e8f5b8f1388740"
  )
})

test_that("package record hashes normalize field order and whitespace", {
  first <- list(
    Package = "example",
    Version = "1.0.0",
    Description = "Some description",
    Imports = c("R6", "digest")
  )
  second <- list(
    Imports = "R6, digest",
    Description = "Some\n  description",
    Version = "1.0.0",
    Package = "example"
  )

  expect_identical(
    Strategus:::.computePackageRecordHash(first),
    Strategus:::.computePackageRecordHash(second)
  )
})

test_that("uninformative remote metadata is omitted", {
  record <- list(
    Package = "example",
    Version = "1.0.0",
    RemoteType = "github",
    RemoteRef = "HEAD",
    RemoteSha = "0123456789012345678901234567890123456789"
  )
  withoutHead <- record
  withoutHead$RemoteRef <- NULL

  expect_identical(
    Strategus:::.computePackageRecordHash(record),
    Strategus:::.computePackageRecordHash(withoutHead)
  )
})

test_that("empty analysis specifications record Strategus identity", {
  specifications <- createEmptyAnalysisSpecifications()

  expect_identical(
    specifications$strategusVersion,
    as.character(utils::packageVersion("Strategus"))
  )
  expect_match(specifications$strategusPackageHash, "^[[:xdigit:]]{32}$")
  expect_identical(specifications$strategusPackageHash, tolower(specifications$strategusPackageHash))
})

test_that("module package identity is recorded when a module is added", {
  module <- CohortGeneratorModule$new()
  packageInformation <- module$getPackageInformation()
  moduleSpecifications <- module$createModuleSpecifications()
  analysisSpecifications <- createEmptyAnalysisSpecifications() |>
    addModuleSpecifications(moduleSpecifications)
  recorded <- analysisSpecifications$moduleSpecifications[[1]]

  expect_identical(recorded$modulePackage, "CohortGenerator")
  expect_identical(
    recorded$modulePackageVersion,
    as.character(utils::packageVersion("CohortGenerator"))
  )
  expect_match(recorded$modulePackageHash, "^[[:xdigit:]]{32}$")
  expect_identical(
    unclass(recorded[c("modulePackage", "modulePackageVersion", "modulePackageHash")]),
    packageInformation
  )
})

test_that("package identity survives JSON serialization", {
  specifications <- createEmptyAnalysisSpecifications() |>
    addCohortGeneratorModuleSpecifications(
      CohortGeneratorModule$new()$createModuleSpecifications()
    )
  json <- ParallelLogger::convertSettingsToJson(specifications)
  roundTrip <- ParallelLogger::convertJsonToSettings(json)

  expect_identical(roundTrip$strategusVersion, specifications$strategusVersion)
  expect_identical(roundTrip$strategusPackageHash, specifications$strategusPackageHash)
  expect_identical(
    roundTrip$moduleSpecifications[[1]]$modulePackageHash,
    specifications$moduleSpecifications[[1]]$modulePackageHash
  )
})

test_that("invalid package identity inputs fail clearly", {
  expect_error(
    Strategus:::.getModulePackageName("NotARegisteredModule"),
    "No underlying package is registered"
  )
  expect_error(
    Strategus:::.getPackageIdentity("NotAnInstalledPackage"),
    "must be installed"
  )
  expect_error(
    Strategus:::.computePackageRecordHash("not a record"),
    "named list"
  )
})
