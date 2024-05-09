# Zip all study results for sharing with study coordinator
#' @export
zipResults <- function(executionSettings, zipFile) {
  checkmate::assert_multi_class(x = executionSettings, classes = c("ExecutionSettings"))
  files <- list.files(
    path = executionSettings$resultsFolder,
    pattern = "\\.log$|\\.csv$"
  )
  DatabaseConnector::createZipFile(
    zipFile = zipFile,
    files = executionSettings$resultsFolder
  )
}
