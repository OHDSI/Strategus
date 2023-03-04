

.handleInjectVar <- function (x) {
  hVar <- function(x) {
    if (is.character(x)) {
      return(sprintf('"%s"', x))
    } else {
      return(sprintf('%s', x))
    }
  }

  if (length(x) == 1) {
    return(hVar(x))
  } else if (is.vector(x)) {
    innerVars <- hVar(x)
    return(paste0("c(", paste(innerVars, collapse = ", "), ")"))
  } else {
    stop("cannot handle complex data structures in variable injection")
  }
}

#' Load module execution space inside and renv
#' inspired by targets::tar_script but allowing custom variable execution
#'
#' Designed to allow more human readable code that is executed inside a module as well as simple variable substituion
#' for injecting constants (e.g. simple parameters or file paths used inside and outside of modules)
runInModuleEnv <- function(code,
                           moduleFolder,
                           injectVars = list(),
                           tempScriptFile = tempfile(fileext = ".R"),
                           useLocalStrategusLibrary = TRUE,
                           job = FALSE,
                           processName = paste(moduleFolder, "_renv_run")) {
  # convert human readable code to a string for writing
  script <- as.character(substitute(code))[-1]
  # Insert variables
  for (name in names(injectVars)) {
    rep <- .handleInjectVar(injectVars[[name]])
    script <- gsub(name, rep, script)
  }

  # Enforce attachment of Strategus from calling process - note one inside the renv
  if (useLocalStrategusLibrary) {
    libPath <- file.path(find.package("Strategus"), "../")
    script <- c(sprintf("library(Strategus, lib.loc = '%s')", libPath),
                script)
  }

  # Write file and execute script inside an renv
  fileConn <- file(tempScriptFile)
  writeLines(script, fileConn)
  close(fileConn)
  renv::run(
    script = tempScriptFile,
    job = job,
    name = processName,
    project = moduleFolder
  )
  return(invisible(NULL))
}