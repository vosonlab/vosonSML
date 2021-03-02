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

    credential <- list(socialmedia = "twitter", auth = NULL, bearer = NULL)
    class(credential) <-
      append(class(credential), c("credential", "twitter"))

    if (missing(appName)) {
      stop("Missing twitter app name.", call. = FALSE)
    }

    if (missing(apiKey) || missing(apiSecret)) {
      stop("Missing twitter consumer API keys.", call. = FALSE)
    }

    # application only keys
    if (missing(accessToken) || missing(accessTokenSecret)) {
      rlang::check_installed("httpuv", "for Authenticate.twitter")

      credential$auth <- rtweet::create_token(
        app = appName,
        consumer_key = apiKey,
        consumer_secret = apiSecret,
        set_renv = FALSE
      )

      return(credential)
    }

    # developer keys
    credential$auth <- rtweet::create_token(
      app = appName,
      consumer_key = apiKey,
      consumer_secret = apiSecret,
      access_token = accessToken,
      access_secret = accessTokenSecret,
      set_renv = FALSE
    )

    credential
  }

#' @title Twitter API v2 authentication
#'
#' @description Twitter API v2 conversation authentication currently only supports app bearer tokens.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate, set to \code{"twitter_convo"}.
#' @param apiKey Character string. API consumer key to authenticate.
#' @param apiSecret Character string. API consumer secret to authenticate.
#' @param bearer Character string. Sets bearer token to string value. If NULL retrieves application bearer token from
#'   API. Default is \code{NULL}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @examples
#' \dontrun{
#' # twitter app keys
#' twitterAuth <- Authenticate("twitter_convo",
#'                              apiKey = "xxxxxxxxxxxx",
#'                              apiSecret = "xxxxxxxxxxxx",
#'                              bearer = NULL)
#'
#' # if already have the app bearer token
#' twitterAuth <- Authenticate("twitter_convo", bearer = "xxxxxxxxxxxx")
#' }
#'
#' @export
Authenticate.twitter_convo <-
  function(socialmedia,
           apiKey,
           apiSecret,
           bearer = NULL,
           ...) {

    credential <- list(socialmedia = "twitter_convo", bearer = NULL)
    class(credential) <-
      append(class(credential), c("credential", "twitter_convo"))

    # bearer token
    if (!is.null(bearer) & is.character(bearer)) {
      credential$bearer <- trimws(bearer)
      return(credential)
    }

    if (missing(apiKey) || missing(apiSecret)) {
      stop("Missing twitter consumer API keys.", call. = FALSE)
    }

    credential$bearer <- get_bearer_token(apiKey, apiSecret)
    credential
  }
