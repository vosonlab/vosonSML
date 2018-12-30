#' Reddit API authentication.
#'
#' OAuth2 based authentication with the Reddit API that returns an authentication token.
#' 
#' The httr package has a known OAuth2 issue with its parameter "use_basic_auth", The default value is set to FALSE
#' and is missing parameter pass through meaning it can not be set to TRUE as required by reddit oauth2 authentication.
#' The point patch devtools::install_github("r-lib/httr#485") fixes this issue.
#' Further information: https://github.com/r-lib/httr/issues/482
#'
#' Reddit oauth tokens are only valid for one hour and using cached token will subsequently produce 401 errors.
#' 
#' @param appName character string containing the reddit app name associated with the API key.
#' @param appKey  character string containing the app key.
#' @param appSecret  character string containing the app secret.
#' @param useCachedToken logical. Use cached authentication token if found.
#' 
#' @return a reddit authentication token
#'
AuthenticateWithRedditAPI <- function(appName, appKey, appSecret, useCachedToken) {

  if (missing(appName)) {
    appName <- "vosonSML-reddit"
  }
  
  if (missing(appKey) | missing(appSecret)) {
    cat("Error. One or more API credentials are missing.\nPlease specify these.\n")
    return()
  }

  if (missing(useCachedToken)) {
    useCachedToken <- FALSE
  }
  
  # sets up oauth2 for reddit
  reddit_endpoint <- httr::oauth_endpoint(
    authorize = "https://www.reddit.com/api/v1/authorize",
    access = "https://www.reddit.com/api/v1/access_token"
  )

  reddit_app <- httr::oauth_app(appName, key = appKey, secret = appSecret)
  
  reddit_token <- httr::oauth2.0_token(reddit_endpoint, reddit_app,
                                       user_params = list(duration = "permanent"),
                                       scope = c("read"),
                                       use_basic_auth = TRUE,
                                       config_init = user_agent("httr oauth"),
                                       cache = useCachedToken)

  return(reddit_token)
}
