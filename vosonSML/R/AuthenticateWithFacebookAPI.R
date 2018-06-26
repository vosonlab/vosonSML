#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Authenticate} function
#'
#' Facebook API Authentication
#'
#' OAuth token based authentication with the Facebook API, with caching options
#' for automatic authentication (i.e. avoid using the browser).
#'
#' In order to collect data from Facebook, the user must first authenticate
#' with Facebook's Application Programming Interface (API). Furthermore, the
#' user must create a Facebook 'app' and get an 'app secret'.
#'
#' To get a Facebook 'app ID' and 'API secret', the excellent tutorial at
#' http://thinktostart.com/analyzing-facebook-with-r/ provides more
#' information.
#'
#' One problem with Facebook authentication through R is that it normally
#' requires the user to authenticate using their browser each time they wish to
#' collect data. The \code{useCachedToken} argument provides a way to
#' circumvent this, by saving and loading an authenticated 'token' file stored
#' in the working directory. If the \code{useCachedToken} argument is set to
#' \code{TRUE}, then the browser is not necessary for future sessions.
#'
#' @param appID character string specifying the 'App ID' of the Facebook app
#' used for authentication.
#' @param appSecret character string specifying the 'API Secret' associated
#' with the Facebook App used for authentication.
#' @param extended_permissions logical. If \code{TRUE} then behaves as
#' described in package 'Rfacebook': the token will give access to some of the
#' authenticated user's private information (birthday, hometown, location,
#' relationships) and that of his/her friends, and permissions to post status
#' updates as well as to access checkins, likes, and the user's newsfeed. If
#' FALSE, token will give access only to public information. Note that
#' updateStatus will only work for tokens with extended permissions.
#' @param useCachedToken logical. If \code{TRUE} then this function will look
#' for a saved token in the current working directory (name of token file must
#' be \code{fb_oauth}). If \code{fb_oauth} token is not found, then it will
#' create a token and save it to current working directory (i.e. for future
#' use).
#' @return An OAuth access token that enables R to make authenticated calls to
#' the Facebook API.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithTwitterAPI} and
#' \code{AuthenticateWithYouTubeAPI} for other ways to collect social media
#' data.
#' @keywords facebook social media SNA
#' @examples
#'
#' \dontrun{
#'   ## Use your own values for myAppID and myAppSecret
#'   myAppID <- "123456789098765"
#'   myAppSecret <- "abc123abc123abc123abc123abc123ab"
#'
#'   # Authenticate with the Facebook API using `AuthenticateWithFacebookAPI`
#'   fb_oauth <- AuthenticateWithFacebookAPI(appID=myAppID, appSecret=myAppSecret,
#'     extended_permissions=FALSE, useCachedToken=TRUE)
#'   }
#' @export
AuthenticateWithFacebookAPI <-
function(appID, appSecret, extended_permissions, useCachedToken) {

  # EnsurePackage("Rfacebook")

  if (missing(extended_permissions)) {
    extended_permissions <- FALSE # default to not using extended permissions
  }

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
    if (file.exists("fb_oauth")) {
cat("\nCached fb_oauth token was found (using cached token).\n") # DEBUG
      load("fb_oauth")
      return(fb_oauth)
    }
    else {
      cat("\nOAuth token `fb_oauth` not found. A token will be created and saved to working directory.\n")
    }
  }

#   ## QUICK FIX
#   # 26/5/17 - unfortunately there is a problem with `fb_oauth` due to api changes.
#   # see https://stackoverflow.com/a/43099356/2589495.
#   # so, we need to define a temporary function that is modified to fix the problem
#
#   rfacebook_oauth_fixed <- function (app_id, app_secret, extended_permissions = FALSE, legacy_permissions = FALSE)
# {
#   full_url <- oauth_callback()
#   full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x = full_url,
#                    replacement = "\\1")
#   message <- paste("Copy and paste into Site URL on Facebook App Settings:",
#                    full_url, "\nWhen done, press any key to continue...")
#   invisible(readline(message))
#   facebook <- oauth_endpoint(authorize = "https://www.facebook.com/dialog/oauth",
#                              access = "https://graph.facebook.com/oauth/access_token")
#   myapp <- oauth_app("facebook", app_id, app_secret)
#   if (extended_permissions == TRUE) {
#     scope <- paste("user_birthday,user_hometown,user_location,user_relationships,",
#                    "publish_actions,user_status,user_likes", collapse = "")
#   }
#   else {
#     scope <- "public_profile,user_friends"
#   }
#   if (legacy_permissions == TRUE) {
#     scope <- paste(scope, "read_stream", sep = ",")
#   }
#   if (packageVersion("httr") <= "0.2") {
#     facebook_token <- oauth2.0_token(facebook, myapp, scope = scope)
#     fb_oauth <- sign_oauth2.0(facebook_token$access_token)
#     if (GET("https://graph.facebook.com/me", config = fb_oauth)$status ==
#         200) {
#       message("Authentication successful.")
#     }
#   }
#   if (packageVersion("httr") > "0.2" & packageVersion("httr") <=
#       "0.6.1") {
#     fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope, cache = FALSE)
#     if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status ==
#         200) {
#       message("Authentication successful.")
#     }
#   }
#   if (packageVersion("httr") > "0.6.1") {
#     Sys.setenv(HTTR_SERVER_PORT = "1410/")
#     fb_oauth <- oauth2.0_token(facebook, myapp, scope = scope, cache = FALSE)
#     if (GET("https://graph.facebook.com/me", config(token = fb_oauth))$status ==
#         200) {
#       message("Authentication successful.")
#     }
#   }
#   error <- tryCatch(callAPI("https://graph.facebook.com/pablobarbera",
#                             fb_oauth), error = function(e) e)
#   if (inherits(error, "error")) {
#     class(fb_oauth)[4] <- "v2"
#   }
#   if (!inherits(error, "error")) {
#     class(fb_oauth)[4] <- "v1"
#   }
#   return(fb_oauth)
# }

  # note that we use the fixed function now, not the original Rfacebook one
  fb_oauth <- fbOAuth(appID, appSecret, extended_permissions)

  if (useCachedToken) {
    save(fb_oauth, file="fb_oauth")
  }

  return(fb_oauth)

}
