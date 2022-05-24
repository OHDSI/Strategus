createCohortGeneratorModuleSpecifications <- function(incremental = TRUE,
                                                      generateStats = TRUE) {
  analysis <- list()
  for (name in names(formals(createCohortGeneratorModuleSpecifications))) {
    analysis[[name]] <- get(name)
  }

  checkmate::assert_file_exists("MetaData.json")
  moduleInfo <- ParallelLogger::loadSettingsFromJson("MetaData.json")
  
  specifications <- list(module = moduleInfo$Name,
                         version = moduleInfo$Version,
                         remoteRepo = "github.com",
                         remoteUsername = "ohdsi",
                         settings = analysis)
  class(specifications) <- c("CohortGeneratorModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}