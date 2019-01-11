#' Note: this function is DEPRECATED. Please use the \code{\link{Authenticate}} function.
#'
#' Twitter API authentication
#'
#' Oauth based authentication using the Twitter API.
#'
#' In order to collect data from Twitter, the user must first authenticate with Twitter's API. This requires setting up 
#' an app on Twitter. A useful guide to creating an app can be found in the rtweet documentation: 
#' https://rtweet.info/articles/auth.html#creating-a-twitter-app
#'
#' @param appName Character string. Specifies the twitter registered app name associated with API keys.
#' @param apiKey Character string. Specifies the app 'API key' used for authentication.
#' @param apiSecret Character string. Specifies the app 'API secret'.
#' @param accessToken Character string. Specifies the app 'access token'.
#' @param accessTokenSecret Character string. Specifies the app 'access token secret'.
#' @param useCachedToken Logical. If \code{TRUE} uses cached API token if found otherwise creates one.
#' 
#' @return twitter_oauth. Returns a twitter oauth token object.
#' 
#' @seealso \code{\link{Authenticate}}
#' @keywords authenticate twitter
#' 
AuthenticateWithTwitterAPI <- function(appName, apiKey, apiSecret, accessToken, accessTokenSecret,
                                        useCachedToken) {

  if (missing(apiKey) | missing(apiSecret) | missing(accessToken) | missing(accessTokenSecret)) {
    cat("Error. One or more API credentials arguments are missing.\nPlease specify these. \n")
    return(NULL)
  }
  
  if (missing(appName)) {
    appName <- "vosonSML-twitter"
  }
  
  twitter_oauth <- NULL
  token_file_name <- ".twitter-oauth"
  
  if (useCachedToken) {
    if (file.exists(token_file_name)) {
      cat("\nCached twitter token was found (using cached token).\n")
      twitter_oauth <- LoadCredential(token_file_name)
      # todo: check loaded token is valid before returning
      return(twitter_oauth)
    } else {
      cat("\nOAuth token not found. A token will be created and saved to working directory.\n")
    }
  }
  
  twitter_oauth <- rtweet::create_token(
    app = appName,
    consumer_key = apiKey,
    consumer_secret = apiSecret,
    access_token = accessToken,
    access_secret = accessTokenSecret,
    set_renv = FALSE)
  
  if (useCachedToken) {
    SaveCredential(twitter_oauth, filename = token_file_name)
  }
  
  return(twitter_oauth)
}
