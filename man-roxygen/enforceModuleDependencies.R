#' @param enforceModuleDependencies When set to TRUE, Strategus will enforce
#' module dependencies that are declared by each module. For example, the
#' CohortDiagnostics module declares a dependency on the CohortGenerator module
#' and Strategus will require that an analysis specification declare that both
#' modules must exist in order to execute the analysis. When set to FALSE,
#' Strategus will not enforce these module dependencies which assumes you have
#' properly run all module dependencies yourself. Setting this to FALSE is not
#' recommended since it is potentially unsafe.
