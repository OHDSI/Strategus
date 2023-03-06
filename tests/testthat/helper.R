skip_if_linux <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])
  if (sysname == "linux") skip("Linux")
  invisible(TRUE)
}
