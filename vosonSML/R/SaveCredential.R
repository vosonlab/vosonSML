#' Save and load credential information
#'
#' Functions to save and load credential information. Currently, credential information will be stored as a RDS file. 
#' \code{SaveCredential} will return the input \code{credential}, useful for working as a filter between the 
#' \code{Authenticate} and \code{Collect}.
#'
#' @aliases SaveCredential LoadCredential
#' 
#' @param credential A \code{credential} object.
#' @param filename Character string. Filename to be saved to or restored from. Default value is \code{credential.RDS}.
#' 
#' @return A \code{credential} object.
#' 
#' @note A \code{credential} created from \code{Authenticate} with \code{socialmedia = "twitter"} will not be saved by 
#' \code{SaveCredential}.
#' 
#' @examples
#' \dontrun{
#' require(magrittr)
#' 
#' ## save credential example
#' 
#' myIgAppID <- "xxxxxxxxxxx"
#' myIgAppSecret <- "xxxxxxxxxxxxxxxxxxxxxx"
#' listIgUsernames <- c("senjohnmccain", "obama")
#'
#' Authenticate("instagram", appID = myIgAppID, appSecret = myIgAppSecret) %>% 
#'   SaveCredential("instagramCred.RDS") %>% 
#'   Collect(ego = TRUE, username = listIgUsernames) %>% Create()
#'
#' ## load previously saved credential example
#' 
#' LoadCredential("instagramCred.RDS") %>% 
#'   Collect(tag = "obama", distance = 5000, n = 100) %>% Create("bimodal")
#' }
#' 
#' @export
SaveCredential <- function(credential, filename = "credential.RDS") {
  if (credential$socialmedia == "twitter") {
    warning("Credential created for Twitter will not be saved.")
  } else {
    saveRDS(credential, filename)
  }
  return(credential)
}

### For the side effect of saving the credential into a file.
### Useful to cache the Credential to a file and then re-use it in the future session.
### i.e. Authenticate %>% SaveCredential %>% Collect
### and then, LoadCredential %>% Collect

#' @rdname SaveCredential
#' @export
LoadCredential <- function(filename = "credential.RDS") {
  credential <- readRDS(filename)
  return(credential)
}
