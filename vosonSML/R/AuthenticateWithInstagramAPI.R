#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Authenticate} function
#'
#' Instagram API Authentication
#'
#' OAuth token based authentication with the Instagram API, with caching
#' options for automatic authentication (i.e. avoid using the browser).
#'
#' In order to collect data from Instagram, the user must first authenticate
#' with Instagram's Application Programming Interface (API). Furthermore, the
#' user must create a Instagram 'app' and get an 'app secret'.
#'
#' To get a Instagram 'app ID' and 'API secret', please see the Instagram
#' document at: https://instagram.com/developer/authentication/
#'
#' One problem with Instagram authentication through R is that it normally
#' requires the user to authenticate using their browser each time they wish to
#' collect data. The \code{useCachedToken} argument provides a way to
#' circumvent this, by saving and loading an authenticated 'token' file stored
#' in the working directory. If the \code{useCachedToken} argument is set to
#' \code{TRUE}, then the browser is not necessary for future sessions.
#'
#' @param appID character string specifying the 'App ID' of the Instagram app
#' used for authentication.
#' @param appSecret character string specifying the 'API Secret' associated
#' with the Instagram App used for authentication.
#' @param useCachedToken logical. If \code{TRUE} then this function will look
#' for a saved token in the current working directory (name of token file must
#' be \code{fb_oauth}). If \code{fb_oauth} token is not found, then it will
#' create a token and save it to current working directory (i.e. for future
#' use).
#' @return An OAuth access token that enables R to make authenticated calls to
#' the Instagram API.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithTwitterAPI} and
#' \code{AuthenticateWithYouTubeAPI} and \code{AuthenticateWithFacebookAPI} for
#' other ways to collect social media data.
#' @keywords Instagram social media SNA
#' @examples
#'
#' \dontrun{
#'   ## Use your own values for myAppID and myAppSecret
#'   app_id <- "123456789098765"
#'   app_secret <- "abc123abc123abc123abc123abc123ab"
#'
#'   # Authenticate with the Instagram API using `AuthenticateWithInstagramAPI`
#'   instagram_oauth_token <- AuthenticateWithInstagramAPI(appID=app_id,
#'     appSecret=app_secret, useCachedToken=FALSE)
#'   }
#' @export
AuthenticateWithInstagramAPI <-
function(appID, appSecret, useCachedToken) {

  # EnsurePackage("instaR")

  #if (missing(appId) | missing(appSecret)) {
  #  cat("Error. One or more API credentials arguments are missing.\nPlease specify these.")
  #  return()
  #}

  if (missing(useCachedToken)) {
# cat("\nuseCachedToken is missing, defaulting to none...") # DEBUG
    useCachedToken <- FALSE # default to not using cached token
  }

  if (useCachedToken=="TRUE" | useCachedToken=="true" | useCachedToken=="T" | useCachedToken==TRUE) {
# cat("\nuseCachedToken is TRUE...") # DEBUG
    useCachedToken <- TRUE # handling user input
  }

  if (useCachedToken) {
    if (file.exists("instagram_oauth_token")) {
cat("\nCached token was found (authentication will use the cached token).\n") # DEBUG
      load("instagram_oauth_token")
      return(instagram_oauth_token)
    }
    else {
      cat("\nOAuth token `instagram_oauth_token` not found. A token will be created and saved to working directory.\n")
    }
  }

  instagram_oauth_token <- instaOAuth(appID, appSecret)

  if (useCachedToken) {
    save(instagram_oauth_token, file="instagram_oauth_token")
  }

  return(instagram_oauth_token)

}
