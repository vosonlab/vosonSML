#' @title Mastodon API authentication
#'
#' @description Mastodon OAuth authentication.
#'
#' @note \pkg{vosonSML} uses the \pkg{rtoot} package for Mastodon data collection and API access tokens.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate, set to \code{"mastodon"}.
#' @param instance Character string. Server to authenticate against and create token.
#' @param type Character string. Type of access, can be \code{"public"} or \code{"user"}. Default is \code{"public"}.
#' @param verbose Logical. Output additional information. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A \code{credential} object containing an access token \code{$auth} and social media type descriptor
#'   \code{$socialmedia} set to \code{"mastodon"}. Object has the class names \code{"credential"} and \code{"mastodon"}.
#'
#' @examples
#' \dontrun{
#' # mastodon API public access bearer token
#' mastodon_auth <- Authenticate(
#'   "mastodon",
#'   instance = "mastodon.social"
#' )
#'
#' # mastodon API user access bearer token
#' mastodon_auth_user <- Authenticate(
#'   "mastodon",
#'   instance = "mastodon.social",
#'   type = "user"
#' )
#' 
#' # if thread collection API token not required
#' mastodon_auth <- Authenticate("mastodon")
#' }
#'
#' @export
Authenticate.mastodon <-
  function(socialmedia,
           instance = NULL,
           type = "public",
           verbose = FALSE,
           ...) {

    prompt_and_stop("rtoot", "Authenticate.mastodon")
    
    msg("Creating mastodon token...\n")

    credential <- list(socialmedia = "mastodon", token = NULL)
    class(credential) <-
      append(class(credential), c("credential", "mastodon"))

    if (is.null(instance)) {
      msg("No instance specified, setting public auth.\n")
      
      credential$auth <- NULL
      
      msg("Done.\n")
      
      return(credential)
    }
    
    instance <- check_chr(instance, param = "instance", min = 1)
    type <- check_chr(type, param = "type", accept = c("public", "user"))
    
    token <- rtoot::auth_setup(
      instance = instance,
      type = type,
      name = NULL,
      path = NULL,
      clipboard = FALSE,
      verbose = TRUE,
      browser = TRUE
    )

    credential$auth <- token

    msg("Done.\n")

    credential
  }

