#' Create a credential to access social media APIs
#'
#' \code{Authenticate} creates a \code{credential} object that enables R to make authenticated calls to social media
#' APIs. A \code{credential} object is a S3 object with the authentication-related information such as access tokens
#' and the information on the social media that grant authentication. \code{Authenticate} is the first step of the
#' \code{Authenticate}, \code{\link{Collect}} and \code{\link{Create}} workflow.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate.\cr
#' Supports: \code{"twitter"}, \code{"youtube"}, \code{"reddit"}, \code{"instagram"} and \code{"facebook"}.
#' @param ... Additional parameters for authentication appropriate to \code{socialmedia} identifier.
#' \describe{
#'   \item{twitter:}{\code{[appName], apiKey, apiSecret, accessToken, 
#'                         accessTokenSecret, [useCachedToken]}}
#'   \item{youtube:}{\code{apiKey}}
#'   \item{reddit:}{\code{[appName], appKey, appSecret, [useCachedToken]}}
#'   \item{instagram:}{\code{appID, appSecret, [useCachedToken]}}
#'   \item{facebook:}{\code{appID, appSecret, [extendedPermissions, useCachedToken]}}
#' }
#'
#' @return A \code{credential} object with authentication information.
#'
#' @note Currently, \code{Authenticate} with \code{socialmedia = "twitter"} generates OAuth information to be used in
#' the current active session only (i.e. "side-effect") and no authentication-related information will be stored in the
#' returned \code{credential} object.
#'
#' For other social network API's it's useful to cache the credential to a file and then re-use it in future sessions.
#' Refer to \code{\link{SaveCredential}} and \code{\link{LoadCredential}} to do this.
#'
#' @seealso \code{\link{SaveCredential}}, \code{\link{Collect}}, \code{\link{Create}}
#' @keywords authenticate credential twitter youtube reddit instagram facebook
#'
#' @examples
#' \dontrun{
#' require(magrittr)
#'
#' ## youtube actor network example
#'
#' myYoutubeAPIKey <- "xxxxxxxxxxxxxxxxxxxxxx"
#' listYoutubeVideoIDs <- c("W2GZFeYGU3s", "mL27TAJGlWc")
#'
#' myActorNetwork <- Authenticate("youtube", apiKey = myYoutubeAPIKey) %>%
#'   Collect(videoIDs = listYoutubeVideoIDs) %>% Create("actor")
#'
#' ## instagram ego network example
#'
#' myInstaAppID <- "xxxxxxxxxxx"
#' myInstaAppSecret <- "xxxxxxxxxxxxxxxxxxxxxx"
#' listInstaUsernames <- c("senjohnmccain", "obama")
#'
#' myEgoNetwork <- Authenticate("instagram", appID = myInstaAppID, appSecret = myInstaAppSecret) %>%
#'   Collect(ego = TRUE, username = listInstaUsernames) %>% Create("ego")
#' }
#'
#' @export
Authenticate <- function(socialmedia, ...) {
  authenticator <- switch(tolower(socialmedia),
                          twitter = twitterAuthenticator,
                          youtube = youtubeAuthenticator,
                          reddit = redditAuthenticator,
                          instagram = instagramAuthenticator,
                          facebook = facebookAuthenticator,
                          stop("Unknown socialmedia"))
  
  auth <- authenticator(...)
  
  credential <- list(socialmedia = tolower(socialmedia), auth = auth)
  class(credential) <- append(class(credential), "credential")
  
  return(credential)
}

twitterAuthenticator <- function(appName, apiKey, apiSecret, accessToken, accessTokenSecret, useCachedToken) {
  return(AuthenticateWithTwitterAPI(appName, apiKey, apiSecret, accessToken, accessTokenSecret, useCachedToken))
}

youtubeAuthenticator <- function(apiKey) {
  return(AuthenticateWithYoutubeAPI(apiKey))
}

redditAuthenticator <- function(appName, appKey, appSecret, useCachedToken) {
  # return(AuthenticateWithRedditAPI(appName, appKey, appSecret, useCachedToken))
  return(NULL)
}

instagramAuthenticator <- function(appID, appSecret) {
  return(AuthenticateWithInstagramAPI(appID, appSecret))
}

facebookAuthenticator <- function(appID, appSecret, extendedPermissions = FALSE) {
  return(AuthenticateWithFacebookAPI(appID, appSecret, extendedPermissions, useCachedToken = FALSE))
}
