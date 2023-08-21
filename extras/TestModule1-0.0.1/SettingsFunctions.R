#' Create specifications for the TestModule1
#'
#' @return
#' An object of type `TestModule1Specifications`.
#'
#' @export
createTestModule1Specifications <- function() {
  analysis <- list()
  for (name in names(formals(createTestModule1Specifications))) {
    analysis[[name]] <- get(name)
  }

  specifications <- list(
    module = "TestModule1",
    version = "0.0.1",
    settings = analysis
  )
  class(specifications) <- c("TestModule1Specifications", "ModuleSpecifications")
  return(specifications)
}
