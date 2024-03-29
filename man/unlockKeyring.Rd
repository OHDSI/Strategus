% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Settings.R
\name{unlockKeyring}
\alias{unlockKeyring}
\title{Helper function to unlock a keyring}
\usage{
unlockKeyring(keyringName)
}
\arguments{
\item{keyringName}{The name of the keyring to operate on. This function assumes you have
created the keyring before calling this function. It defaults to
NULL to select the default keyring. If the keyring is password
protected, the password must be stored in the environment variable
STRATEGUS_KEYRING_PASSWORD so it is retrieved using the command
Sys.getenv("STRATEGUS_KEYRING_PASSWORD")}
}
\value{
Returns TRUE if the keyring was unlocked using the password otherwise
it returns FALSE
}
\description{
This helper function is used to unlock a keyring by using the password
stored in Sys.getenv("STRATEGUS_KEYRING_PASSWORD"). It will alert
the user if the environment variable with the password is not set.
}
