#' @param forceVerification When set to TRUE, the verification process is forced
#' to re-evaluate if a module is properly installed. The default is FALSE
#' since if a module is successfully validated, the module will contain
#' the hash value of the module's renv.lock file in the file system so it can
#' by-pass running this check every time.
