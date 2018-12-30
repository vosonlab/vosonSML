#' Save and load credential information
#'
#' Functions to save and load credential information. Currently, credential information will be stored as a RDS file. 
#' \code{SaveCredential} will return the input \code{credential}, useful for working as a filter between 
#' \code{Authenticate} and \code{Collect}.
#'
#' @aliases SaveCredential LoadCredential
#' 
#' @param credential A \code{credential} object.
#' @param filename Character string. Filename to be saved to or restored from. Default value is \code{credential.RDS}.
#' 
#' @return A \code{credential} object.
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
SaveCredential <- function(credential, filename) {
  if (missing(credential) || missing(filename)) {
    stop("please supply a credential object and credential file name to save.")
  }  
  saveRDS(credential, filename)
  return(credential)
}

#' @rdname SaveCredential
#' @export
LoadCredential <- function(filename) {
  if (missing(filename)) {
    stop("please supply a credential file name to load.")
  }    
  credential <- readRDS(filename)
  return(credential)
}
