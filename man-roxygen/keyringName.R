#' @param keyringName   The name of the keyring to operate on. This function assumes you have
#'                      created the keyring before calling this function. It defaults to
#'                      NULL to select the default keyring. If the keyring is password
#'                      protected, the password must be stored in the environment variable
#'                      STRATEGUS_KEYRING_PASSWORD so it is retrieved using the command
#'                      Sys.getenv("STRATEGUS_KEYRING_PASSWORD")

