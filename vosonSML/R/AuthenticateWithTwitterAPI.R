#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Authenticate} function
#'
#' Twitter API Authentication
#'
#' Oauth based authentication with the Twitter API
#'
#' In order to collect data from Twitter, the user must first authenticate with
#' Twitter's Application Programming Interface (API).
#'
#' This requires setting up an App on Twitter. An excellent guide to achieving
#' this can be found at:
#' http://thinktostart.com/twitter-authentification-with-r/
#'
#' @param api_key character string specifying the 'API key' used for
#' authentication.
#' @param api_secret character string specifying the 'API secret' used for
#' authentication.
#' @param access_token character string specifying the 'access token' used for
#' authentication.
#' @param access_token_secret character string specifying the 'access token
#' secret' used for authentication.
#' @param createToken logical. !! NOT PROPERLY IMPLEMENTED YET.
#' @return This is called for its side effect.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithFacebookAPI} and
#' \code{AuthenticateWithYouTubeAPI} for other ways to collect social media
#' data.
#' @keywords twitter social media SNA
#' @examples
#'
#' \dontrun{
#'   # Firstly specify your API credentials
#'   my_api_key <- "1234567890qwerty"
#'   my_api_secret <- "1234567890qwerty"
#'   my_access_token <- "1234567890qwerty"
#'   my_access_token_secret <- "1234567890qwerty"
#'
#'   AuthenticateWithTwitterAPI(api_key=my_api_key, api_secret=my_api_secret,
#'     access_token=my_access_token, access_token_secret=my_access_token_secret)
#' }
#' @export
AuthenticateWithTwitterAPI <-
function(api_key, api_secret, access_token, access_token_secret, createToken) {

  # EnsurePackage("tm") # we only load packages as required (i.e. if user authenticate with twitter, then we load packages for twitter data collection/analysis)
  # EnsurePackage("stringr")
  # EnsurePackage("twitteR")
  # EnsurePackage("RCurl")
  # EnsurePackage("bitops")
  # EnsurePackage("rjson")
  # EnsurePackage("plyr")
  # EnsurePackage("igraph")

  if (missing(api_key) | missing(api_secret) | missing(access_token) | missing(access_token_secret)) {
    cat("Error. One or more API credentials arguments are missing.\nPlease specify these. \n")
    return()
  }

  # We avoid the popup prompt about cached authentication,
  # and instead include a `createToken` argument in the function,
  # and directly set the options parameter for the "httr" package.
  # (And default to no token if the argument is missing)

  origOptions <- options("httr_oauth_cache") # original options setting

  if (missing(createToken)) {
    createToken <- FALSE # default to no token
  }

  if (createToken=="TRUE" | createToken=="true" | createToken=="T" | createToken==TRUE) {
    createToken <- TRUE # handling user input
  }

  if (createToken) {
    options(httr_oauth_cache=T)
  }
  else {
    options(httr_oauth_cache=F)
  }

  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

  options(httr_oauth_cache=origOptions) # reset options back to the original setting

  return()

}
