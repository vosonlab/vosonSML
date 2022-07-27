#' @title Twitter API authentication
#'
#' @description Twitter authentication uses OAuth and typically requires four developer API keys generated when you
#'   create a twitter app via the twitter developer web site.
#'
#'   There is another method available commonly used by third-party apps in which an app can be authorized by a user to
#'   use the twitter API on their behalf. The implementation of this method in vosonSML does not require a developer
#'   account but does still require the user to have access to a developers apps two consumer API keys. This allows
#'   multiple users to access the twitter API with vosonSML via a single developer account and app.
#'
#'   The twitter OAuth process is described here:
#'   \url{https://developer.twitter.com/en/docs/basics/authentication/overview/oauth}.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate, set to \code{"twitter"}.
#' @param appName Character string. Registered twitter app name associated with the API keys.
#' @param apiKey Character string. API consumer key to authenticate.
#' @param apiSecret Character string. API consumer secret to authenticate.
#' @param accessToken Character string. API access token to authenticate.
#' @param accessTokenSecret Character string. API access token secret to authenticate.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A \code{credential} object containing an access token \code{$auth} and social media type descriptor
#'   \code{$socialmedia} set to \code{"twitter"}. Object has the class names \code{"credential"} and \code{"twitter"}.
#'
#' @examples
#' \dontrun{
#' # twitter authentication using developer app API keys
#' myDevKeys <- list(appName = "My App", apiKey = "xxxxxxxxxxxx",
#'   apiSecret = "xxxxxxxxxxxx", accessToken = "xxxxxxxxxxxx",
#'   accessTokenSecret = "xxxxxxxxxxxx")
#'
#' twitterAuth <- Authenticate("twitter", appName = myDevKeys$appName,
#'   apiKey = myDevKeys$apiKey, apiSecret = myDevKeys$apiSecret, accessToken = myDevKeys$accessToken,
#'   accessTokenSecret = myDevKeys$accessTokenSecret)
#'
#' # twitter authentication via authorization of an app to their user account
#' # requires the apps consumer API keys
#' # apiKey and apiSecret parameters are equivalent to the apps consumer key and secret
#' # will open a web browser to twitter prompting the user to log in and authorize the app
#' twitterAuth <- Authenticate("twitter", appName = "An App",
#'   apiKey = "xxxxxxxxxxxx", apiSecret = "xxxxxxxxxxxx"
#' )
#' }
#'
#' @export
Authenticate.twitter <-
  function(socialmedia,
           appName,
           apiKey,
           apiSecret,
           accessToken,
           accessTokenSecret,
           ...) {
    rlang::check_installed("rtweet", "for Authenticate.twitter")
    stop_req_pkgs(c("rtweet"), "Authenticate.twitter")

    credential <-
      list(socialmedia = "twitter",
           auth = NULL,
           bearer = NULL)
    class(credential) <-
      append(class(credential), c("credential", "twitter"))

    if (missing(apiKey) || missing(apiSecret)) {
      stop("Missing twitter consumer API keys.", call. = FALSE)
    }

    app <- httr::oauth_app(
      appName,
      key = apiKey,
      secret = apiSecret
    )

    # for user context
    if (missing(accessToken) || missing(accessTokenSecret)) {
      rlang::check_installed("httpuv", "for Authenticate.twitter")

      token <- TwitterToken1.0$new(
        app = app,
        endpoint = httr::oauth_endpoints("twitter"),
        params = list(as_header = TRUE),
        cache_path = FALSE
      )

      credential$auth <- token

      return(credential)
    }

    # for developer context
    credentials <- list(
      oauth_token = accessToken,
      oauth_token_secret = accessTokenSecret
    )

    token <- httr::Token1.0$new(
      app = app,
      endpoint = httr::oauth_endpoints("twitter"),
      params = list(as_header = TRUE),
      credentials = credentials,
      cache_path = FALSE
    )

    credential$auth <- token

    credential
  }

# twitter R6 token class and functions from the ropensci rtweet package on github by @mkearney, @hadley
# duplicated as are not exported by rtweet

TwitterToken1.0 <- R6::R6Class("TwitterToken1.0", inherit = httr::Token1.0, list(
  init_credentials = function(force = FALSE) {
    self$credentials <- twitter_init_oauth1.0(
      self$endpoint,
      self$app,
      permission = self$params$permission,
      private_key = self$private_key
    )
  }
))

twitter_init_oauth1.0 <- function(endpoint, app, permission = NULL,
                                  is_interactive = interactive(),
                                  private_key = NULL) {

  withr::local_envvar("HTTR_SERVER" = "127.0.0.1")
  httr::init_oauth1.0(
    endpoint,
    app,
    permission = permission,
    is_interactive = is_interactive,
    private_key = private_key
  )
}
