skip_if_not_win <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])
  if (sysname != "windows") skip("Not windows")
  invisible(TRUE)
}

skip_if_not_linux <- function() {
  sysname <- tolower(Sys.info()[["sysname"]])
  if (sysname != "linux") skip("Not Linux")
  invisible(TRUE)
}
