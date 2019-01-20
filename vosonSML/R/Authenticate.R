#' Create a credential to access social media APIs
#'
#' \code{Authenticate} creates a \code{credential} object that enables R to make authenticated calls to social media
#' APIs. A \code{credential} object is a S3 object with the authentication-related information such as access tokens
#' and the information on the social media that grant authentication. \code{Authenticate} is the first step of the
#' \code{Authenticate}, \code{\link{Collect}} and \code{\link{Create}} workflow.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate.\cr
#' Supports: \code{"twitter"}, \code{"youtube"}, \code{"reddit"}.
#' @param ... Additional parameters for authentication appropriate to \code{socialmedia} identifier.
#' \describe{
#'   \item{twitter:}{\code{[appName], apiKey, apiSecret, accessToken, accessTokenSecret,}\cr
#'                   \code{[useCachedToken]}}
#'   \item{youtube:}{\code{apiKey}}
#'   \item{reddit:}{\code{No credentials required.}}
#' }
#'
#' @return A \code{credential} object with authentication information.
#'
#' @seealso \code{\link{Collect}}, \code{\link{Create}}
#' @keywords authenticate credential twitter youtube reddit
#'
#' @export
Authenticate <- function(socialmedia, ...) {
  authenticator <- switch(tolower(socialmedia),
                          twitter = twitterAuthenticator,
                          youtube = youtubeAuthenticator,
                          reddit = redditAuthenticator,
                          stop("Authenticate called with unsupported social media type.", call. = FALSE))
  
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
