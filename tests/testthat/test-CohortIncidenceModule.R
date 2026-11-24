test_that("CohortIncidence Andromeda results are exported in batches", {
  skip_if_not_installed("Andromeda", minimum_version = "1.0.0")
  skip_if_not_installed("readr")

  executeResults <- Andromeda::andromeda(
    incidence_summary = data.frame(
      persons_at_risk_pe = c(2, 10),
      persons_at_risk = c(3, 11),
      person_outcomes_pe = c(4, 12),
      person_outcomes = c(2, 13),
      outcomes_pe = c(1, 14),
      outcomes = c(3, 15),
      incidence_rate_p100py = c(1.5, 2.5),
      incidence_proportion_p100p = c(3.5, 4.5)
    ),
    target_def = data.frame(id = c(1, 2), name = c("Target 1", "Target 2")),
    subgroup_def = data.frame(id = integer(), name = character())
  )
  on.exit(Andromeda::close(executeResults), add = TRUE)

  exportFolder <- tempfile("cohort-incidence-export-")
  dir.create(exportFolder)
  on.exit(unlink(exportFolder, recursive = TRUE, force = TRUE), add = TRUE)

  module <- CohortIncidenceModule$new()
  module$.__enclos_env__$private$.exportAndromedaResults(
    executeResults = executeResults,
    exportFolder = exportFolder,
    minCellCount = 5,
    databaseId = "database-1",
    batchSize = 1
  )

  incidenceSummary <- readr::read_csv(
    file.path(exportFolder, "ci_incidence_summary.csv"),
    show_col_types = FALSE
  )
  expect_equal(nrow(incidenceSummary), 2)
  expect_equal(incidenceSummary$PERSONS_AT_RISK_PE, c(-5, 10))
  expect_equal(incidenceSummary$INCIDENCE_RATE_P100PY, c(NA, 2.5))
  expect_equal(incidenceSummary$INCIDENCE_PROPORTION_P100P, c(NA, 4.5))
  expect_equal(incidenceSummary$database_id, rep("database-1", 2))

  targetDef <- readr::read_csv(
    file.path(exportFolder, "ci_target_def.csv"),
    show_col_types = FALSE
  )
  expect_equal(nrow(targetDef), 2)
  expect_equal(targetDef$name, c("Target 1", "Target 2"))

  subgroupDef <- readr::read_csv(
    file.path(exportFolder, "ci_subgroup_def.csv"),
    show_col_types = FALSE
  )
  expect_equal(nrow(subgroupDef), 0)
  expect_named(subgroupDef, c("id", "name"))
})
