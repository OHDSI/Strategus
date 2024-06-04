test_that("Prevent multiple modules with different versions in analysis specification", {
  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = system.file("testdata/unitTestAnalysisSpecification.json",
      package = "Strategus"
    )
  )

  # Duplicate the module
  analysisSpecifications$moduleSpecifications[[2]] <- analysisSpecifications$moduleSpecifications[[1]]
  analysisSpecifications$moduleSpecifications[[2]]$version <- "x"

  expect_error(
    ensureAllModulesInstantiated(
      analysisSpecifications = analysisSpecifications
    )
  )
})

# TODO: We'd like to test this functionality but both methods require that
# the module is instantiated which is very time consuming. Instead these
# tests should mock the MetaData.json that exists in the instantiated
# modules so that these methods work faster.
# test_that("Enforce module dependencies", {
#   analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
#     fileName = system.file("testdata/analysisSpecification.json",
#                            package = "Strategus"
#     )
#   )
#
#   # Remove the cohort generator module which is a dependency for all
#   # the other modules
#   analysisSpecifications$moduleSpecifications <- analysisSpecifications$moduleSpecifications[-1]
#   modules <- getModuleTable(analysisSpecifications, distinct = TRUE)
#
#   expect_error(
#     checkModuleDependencies(
#       modules = modules,
#       enforceModuleDependencies = TRUE
#     )
#   )
# })
#
# test_that("Do not enforce module dependencies", {
#   analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
#     fileName = system.file("testdata/unitTestAnalysisSpecification.json",
#                            package = "Strategus"
#     )
#   )
#
#   modules <- getModuleTable(analysisSpecifications, distinct = TRUE)
#
#   expect_silent(
#     checkModuleDependencies(
#       modules = modules,
#       enforceModuleDependencies = FALSE
#     )
#   )
# })
