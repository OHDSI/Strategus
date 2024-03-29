% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RenvHelpers.R
\name{validateLockFile}
\alias{validateLockFile}
\title{Validate an renv.lock file to ensure it is ready for use by Strategus}
\usage{
validateLockFile(filename)
}
\arguments{
\item{filename}{The renv.lock file to validate}
}
\description{
Will check an renv.lock file for a module to verify that it only references
tagged packages and includes the packages required by Strategus. It will
also check for suggested packages that are useful for testing, such as
RSQLite.
}
