#' Create a zip file with all study results for sharing with study coordinator
#'
#' @details
#' Creates a `.zip` file of the `.csv` files found in the
#' `resultsFolder`. The resulting `.zip` file will have
#' relative paths to the root of the `resultsFolder`
#' which is generally found in `executionSettings$resultsFolder`.
#'
#' @param resultsFolder The folder holding the study results. This is found in
#' `executionSettings$resultsFolder`.
#'
#' @param zipFile The path to the zip file to be created.
#'
#' @return
#' Does not return anything. Is called for the side-effect of creating the
#' zip file with results.
#'
#' @export
zipResults <- function(resultsFolder, zipFile) {
  files <- list.files(
    path = resultsFolder,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  DatabaseConnector::createZipFile(
    zipFile = zipFile,
    files = files,
    rootFolder = resultsFolder
  )
  message(zipFile, " created.")
}
