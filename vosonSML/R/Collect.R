#' Collect data from social media for generating networks
#'
#' This function collects data from social media APIs, and structures the data into a data frame of class
#' \code{dataSource.*}, ready for creating networks for further analysis. \code{Collect} is the second step of the
#' \code{Authenticate}, \code{Collect}, \code{Create} workflow. This function is a convenient UI wrapper to the core
#' CollectDataFrom* family of functions.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate}.
#' @param ego Logical. If \code{TRUE} collect ego network data. Currently only supports Instagram.
#' @param ... Additional parameters for data collection by appropriate to credential \code{socialmedia} type.
#' Refer to CollectDataFrom* and CollectEgo* functions for more details.
#' \describe{
#'   \item{twitter:}{\code{authToken, searchTerm, [searchType, numTweets, includeRetweets, retryOnRateLimit,}\cr
#'                   \code{writeToFile, verbose, ...]}}
#'   \item{youtube:}{\code{videoIDs, apiKeyYoutube, [verbose, writeToFile, maxComments]}}
#'   \item{reddit:}{\code{threadUrls, [waitTime, writeToFile]}}
#'   \item{instagram:}{\code{tag, n, lat, lng, [distance, folder, mindate, maxdate, verbose, sleep,}\cr
#'                     \code{writeToFile, waitForRateLimit, credential]}}
#'   \item{instagram with \code{ego = TRUE}:}{\code{username, userid, [verbose, degreeEgoNet,}\cr
#'                                            \code{waitForRateLimit, getFollows, credential]}}
#'   \item{facebook:}{\code{pageName, [rangeFrom, rangeTo, verbose, n, writeToFile, dynamic]}}
#' }
#'
#' @return A data.frame object of class \code{dataSource.*} that can be used with \code{Create}.
#'
#' @seealso \code{Authenticate}, \code{Create}
#' @keywords collect twitter youtube reddit instagram facebook
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
#'
#' ## facebook bimodal network example
#'
#' myFacebookAppID <- "xxxxxxxxxxx"
#' myFacebookAppSecret <- "xxxxxxxxxxxxxxxxxxxxxx"
#'
#' myBimodalNetwork <- Authenticate("Facebook", appID = myFacebookAppID,
#'                                  appSecret = myFacebookAppSecret) %>%
#'   SaveCredential("FBCredential.RDS") %>%
#'   Collect(pageName = "StarWars", rangeFrom = "2015-03-01", rangeTo = "2015-03-02",
#'           writeToFile = FALSE) %>%
#'   Create("bimodal")
#'
#' ## facebook dynamic network example
#'
#' myDynamicNetwork <- LoadCredential("FBCredential.RDS") %>%
#'   Collect(pageName = "StarWars", rangeFrom = "2015-03-01", rangeTo = "2015-03-02",
#'           writeToFile = FALSE) %>%
#'   Create("dynamic")
#' }
#'
#' @export
Collect <- function(credential, ego = FALSE, ...) {
  if (ego) {
    collector <- switch(credential$socialmedia,
                        instagram = instagramEgo,
                        stop("Unsupported socialmedia"))
  } else {
    collector <- switch(credential$socialmedia,
                        twitter = twitterCollector,
                        youtube = youtubeCollector,
                        reddit = redditCollector,
                        instagram = instagramCollector,
                        facebook = facebookCollector,
                        stop("Unsupported socialmedia"))
  }
  
  return(collector(credential, ...))
}

twitterCollector <- function(credential, ...) {
  return(CollectDataTwitter(authToken = credential$auth, ...))
}

# twitterCollector <- function(credential, searchTerm, searchType, numTweets, includeRetweets, retryOnRateLimit, 
#                               writeToFile, verbose, ...) {
#   return(CollectDataTwitter(authToken = credential$auth, searchTerm, searchType, numTweets, includeRetweets, 
#                             retryOnRateLimit, writeToFile, verbose, ...))
# }

youtubeCollector <- function(credential, ...) {
  return(CollectDataYoutube(apiKey = credential$auth, ...))
}

# youtubeCollector <- function(credential, videoIDs, verbose, writeToFile, maxComments) {
#   return(CollectDataYoutube(videoIDs, apiKeyYoutube = credential$auth, verbose, writeToFile, maxComments))
# }

redditCollector <- function(credential, threadUrls, waitTime, writeToFile) {
  return(CollectDataReddit(threadUrls, waitTime, writeToFile))
}

instagramCollector <- function(credential, tag, n, lat, lng, distance, folder, mindate, maxdate, verbose, sleep,
                               writeToFile, waitForRateLimit) {
  return(CollectDataInstagram(tag, n, lat, lng, distance, folder, mindate, maxdate, verbose, sleep, writeToFile,
                              waitForRateLimit, credential))
}

instagramEgo <- function(credential, username, userid, verbose, degreeEgoNet, waitForRateLimit, getFollows) {
  return(CollectEgoInstagram(username, userid, verbose, degreeEgoNet, waitForRateLimit, getFollows, credential))
}

facebookCollector <- function(credential, pageName, rangeFrom, rangeTo, verbose, n, writeToFile) {
  return(CollectDataFacebook(pageName, rangeFrom, rangeTo, verbose, n, writeToFile, credential))
}
