#' Save and load credential information
#'
#' Functions to save and load credential information. Currently, credential information will be stored as a RDS file. 
#' \code{SaveCredential} will return the input \code{credential}, useful for working as a filter between 
#' \code{Authenticate} and \code{Collect}.
#'
#' @aliases SaveCredential LoadCredential
#' 
#' @param credential A \code{credential} object.
#' @param file Character string. File name to be saved to or restored from.
#' 
#' @return A \code{credential} object.
#' 
#' @export
SaveCredential <- function(credential, file) {
  if (missing(credential) || missing(file)) {
    stop("Please supply a credential object and credential file name to save.", .call = FALSE)
  }  
  saveRDS(credential, file)
  
  return(credential)
}

#' @rdname SaveCredential
#' @export
LoadCredential <- function(file) {
  if (missing(file)) {
    stop("Please supply a credential file name to load.", .call = FALSE)
  }    
  credential <- readRDS(file)
  
  return(credential)
}
