#' Reddit API Authentication.
#'
#' Oauth2 based authentication with the Reddit API that returns an authentication token.
#' 
#' The httr package has a known oauth2 issue with its parameter "use_basic_auth", The default value is set to FALSE
#' and is missing parameter pass through meaning it can not be set to TRUE as required by reddit oauth2 authentication.
#' The point patch devtools::install_github("r-lib/httr#485") fixes this issue.
#' Further information: https://github.com/r-lib/httr/issues/482
#'
#' Reddit oauth tokens are only valid for one hour and using cached token will subsequently produce 401 errors.
#' 
#' @param app_name Character string containing the reddit app name associated with the API key.
#' @param app_key  Character string containing the app key.
#' @param app_secret  Character string containing the app secret.
#' @param use_token_cache Boolean. Use cached authentication token if found.
#' @return Reddit authentication token.
#' 
#' @noRd
authenticateWithRedditAPI <- function(app_name, app_key, app_secret, use_token_cache) {

  if (missing(app_name)) {
    app_name <- "reddit"
  }
  
  if (missing(app_key) | missing(app_secret)) {
    cat("Error. One or more API credentials are missing.\nPlease specify these.\n")
    return()
  }

  if (missing(use_token_cache)) {
    use_token_cache <- FALSE
  }
  
  # sets up oauth2 for reddit
  reddit_endpoint <- httr::oauth_endpoint(
    authorize = "https://www.reddit.com/api/v1/authorize",
    access = "https://www.reddit.com/api/v1/access_token"
  )

  reddit_app <- httr::oauth_app(app_name, key = app_key, secret = app_secret)
  
  reddit_token <- httr::oauth2.0_token(reddit_endpoint, reddit_app,
                                       user_params = list(duration = "permanent"),
                                       scope = c("read"),
                                       use_basic_auth = TRUE,
                                       config_init = user_agent("httr oauth"),
                                       cache = use_token_cache)

  return(reddit_token)
}
