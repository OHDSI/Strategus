#' Partition big json into smaller ones
#'
#' @template AnalysisSpecifications
#' @param specificationFolder A folder to save the modules' partitioned jsons to
#'
#' @return
#' Nothing but in specificationFolder there will be a folder per module in analysisSpecifications
#' that contains json files with the partitioned analysis
#'
#' @export
partitionModule <- function(analysisSpecifications, specificationFolder) {
  
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(analysisSpecifications, "AnalysisSpecifications", add = errorMessages)
  checkmate::assertClass(specificationFolder, "character" , add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  
  for (i in 1:length(analysisSpecifications$moduleSpecifications)) {
    moduleName <- analysisSpecifications$moduleSpecifications[[i]]$module
    modulePartitionStatus <- .partitionModule(
      moduleName = moduleName,
      analysisSpecifications = analysisSpecifications,
      specificationFolder = specificationFolder
    )
  }
}

.partitionModule <- function(moduleName, analysisSpecifications, specificationFolder) {
  
  moduleObject <- get(moduleName)$new()
  
  # save partitioned jsons per module into specificationFolder with each
  # moduleName as a subfolder and then spec_i.jsons in those
  partitionResult <- moduleObject$partitionModuleSpecifications(
    analysisSpecifications = analysisSpecifications, 
    specificationFolder = file.path(specificationFolder, moduleName)
  )
  
  #if (executionResult$status == "FAILED") {
  #  .printErrorMessage(executionResult$error$message)
  #}
  invisible(return(TRUE))
}