#' @title Twitter API authentication
#' 
#' @description Twitter authentication uses OAuth and requires Application-user authentication API keys as described here: 
#' \url{https://developer.twitter.com/en/docs/basics/authentication/overview/oauth}.
#' 
#' @param socialmedia Character string. Identifier for social media API to authenticate, set to \code{"twitter"}.
#' @param appName Character string. Registered twitter app name associated with the API keys.
#' @param apiKey Character string. API key to authenticate.
#' @param apiSecret Character string. API secret to authenticate.
#' @param accessToken Character string. API access token to authenticate.
#' @param accessTokenSecret Character string. API access token secret to authenticate.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return A \code{credential} object containing an access token \code{$auth} and social media type descriptor 
#' \code{$socialmedia} set to \code{"twitter"}. Object has the class names \code{"credential"} and \code{"twitter"}.
#' 
#' @examples
#' \dontrun{
#' # twitter authentication with api keys
#' myKeys <- list(appName = "vosonSML", apiKey = "xxxxxxxxxxxx",
#'   apiSecret = "xxxxxxxxxxxx", accessToken = "xxxxxxxxxxxx",
#'   accessTokenSecret = "xxxxxxxxxxxx")
#' 
#' twitterAuth <- Authenticate("twitter", appName = myKeys$appName, 
#'   apiKey = myKeys$apiKey, apiSecret = myKeys$apiSecret, accessToken = myKeys$accessToken, 
#'   accessTokenSecret = myKeys$accessTokenSecret)
#' }
#' 
#' @export
Authenticate.twitter <- function(socialmedia, appName, apiKey, apiSecret, accessToken, accessTokenSecret, ...) {
  
  if (missing(apiKey) || missing(apiSecret) || missing(accessToken) || missing(accessTokenSecret)) {
    stop("Missing one or more twitter API keys.", call. = FALSE)
  }
  
  if (missing(appName)) {
    appName <- "vosonSML-twitter"
  }
  
  twitter_oauth <- NULL
  token_file_name <- ".twitter-oauth"
  
  credential <- list(socialmedia = "twitter", auth = NULL)
  class(credential) <- append(class(credential), c("credential", "twitter"))
  
  twitter_oauth <- rtweet::create_token(
    app = appName,
    consumer_key = apiKey,
    consumer_secret = apiSecret,
    access_token = accessToken,
    access_secret = accessTokenSecret,
    set_renv = FALSE)
  
  credential$auth <- twitter_oauth
  
  return(credential)
}
