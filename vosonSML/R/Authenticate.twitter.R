#' Twitter API authentication
#' 
#' @param appName Character string. Specifies the registered app name associated with API keys.
#' @param apiSecret Character string. Specifies the app API secret.
#' @param accessToken Character string. Specifies the app access token.
#' @param accessTokenSecret Character string. Specifies the app access token secret.
#' @param useCachedToken Logical. Use cached API token if found otherwise creates one. Default is \code{TRUE}.
#' 
#' @rdname Authenticate
#' @export
Authenticate.twitter <- function(socialmedia, appName, apiKey, apiSecret, accessToken, accessTokenSecret,
                                 useCachedToken = TRUE, ...) {
  
  if (missing(apiKey) || missing(apiSecret) || missing(accessToken) || missing(accessTokenSecret)) {
    stop("Missing one or more twitter API keys.", call. = FALSE)
  }
  
  if (missing(appName)) {
    appName <- "vosonSML-twitter"
  }
  
  twitter_oauth <- NULL
  token_file_name <- ".twitter-oauth"
  
  if (useCachedToken) {
    if (file.exists(token_file_name)) {
      cat("Cached twitter token was found (using cached token).\n")
      twitter_oauth <- LoadCredential(token_file_name)
      # todo: check loaded token is valid before returning
      return(twitter_oauth)
    } else {
      cat("OAuth token not found. A token will be created and saved to the working directory.\n")
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
  
  credential <- list(socialmedia = "twitter", auth = twitter_oauth)
  class(credential) <- append(class(credential), c("credential", "twitter"))
  return(credential)
}
