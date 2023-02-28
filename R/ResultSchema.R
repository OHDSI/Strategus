#' Create results data model
#' @description
#' Create the results model for specified modules with speciefied results database
#'
#' @param resultsExecutionSettings An object of type `ResultsExecutionSettings` as created
#'                                by [createResultsExecutionSettings()].
#'
#' @template keyringName
#'
#' @param restart                 Restart run? Requires `executionScriptFolder` to be specified, and be
#'                                the same as the `executionScriptFolder` used in the run to restart.
#'
#'
#' @return
#' Does not return anything. Is called for the side-effect of executing the specified
#' analyses.
#'
createResultModel <- function(analysisSpecifications,
                              resultsExecutionSettings,
                              keyringName = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  keyringList <- keyring::keyring_list()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(resultsExecutionSettings, "ResultsExecutionSettings", add = errorMessages)
  checkmate::assertChoice(x = keyringName, choices = keyringList$keyring, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  # connectionDetails = jobContext$moduleExecutionSettings$resultsConnectionDetails,
  #                                       schema = jobContext$moduleExecutionSettings$resultsDatabaseSchema,
  #                                       tablePrefix = jobContext$moduleExecutionSettings$resultsTablePrefix, #moduleMetaData$TablePrefix
  createResultsSchema(connectionDetails = connectionDetails, tablePrefix)
}

createResultsSchema <- function(connectionDetaials, schema, tablePrefix, resultModelSpecifications) {
  connection <- DatabaseConnector::connect(connectionDetaials)
  on.exit(DatabaseConnector::disconnect(connection))
  sql <- ResultModelManager::generateSqlSchema(schemaDefinition = resultModelSpecifications)
  DatabaseConnector::renderTranslateExecuteSql(connection, sql, databse_schema = schema, tablePrefix = tablePrefix)
}
