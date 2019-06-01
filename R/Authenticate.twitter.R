#' @title Twitter API authentication
#' 
#' @description Twitter authentication uses OAuth and either requires authorization of the rtweet package rstats2twitter
#' client app by a registered twitter user or twitter app developer API keys as described here: 
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
#' # twitter authentication via user authorization of rstats2twitter app
#' # will open a web browser to twitter prompting the user to log in and authorize the app
#' twitterAuth <- Authenticate("twitter")
#' 
#' # twitter authentication with app developer api keys
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
  credential <- list(socialmedia = "twitter", auth = NULL)
  class(credential) <- append(class(credential), c("credential", "twitter"))   
  
  if (missing(apiKey) & missing(apiSecret) & missing(accessToken) & missing(accessTokenSecret)) {
    credential$auth <- rtweet:::rstats2twitter_client()
    
    return(credential)
  }
  
  if (missing(appName)) {
    stop("Missing twitter app name.", call. = FALSE)
  }
  
  if (missing(apiKey) || missing(apiSecret) || missing(accessToken) || missing(accessTokenSecret)) {
    stop("Missing one or more twitter API keys.", call. = FALSE)
  }
  
  credential$auth <- rtweet::create_token(
    app = appName,
    consumer_key = apiKey,
    consumer_secret = apiSecret,
    access_token = accessToken,
    access_secret = accessTokenSecret,
    set_renv = FALSE)
  
  return(credential)
}
