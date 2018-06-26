#' Collect data from social media for generating networks
#'
#' This function collects data from social media APIs, and structures the data
#' into a data frame of class \code{dataSource.*}, ready for creating networks
#' for further analysis. \code{Collect} is the second step of the
#' \code{Authenticate}, \code{Collect}, \code{Create} workflow. This function is
#' a convenient UI wrapper to the core CollectDataFrom* family of functions.
#'
#'
#' @param credential \code{credential} object generated from
#' \code{Authenticate}
#' @param ego logical, collecting ego network data. Currently only support
#' Instagram.
#' @param ... additional parameters for data collection (refer to
#' CollectDataFrom* and CollectEgo* functions)
#'
#' \code{facebook}: pageName, rangeFrom, rangeTo, verbose, n, writeToFile,
#' dynamic
#'
#' \code{youtube}: videoIDs, verbose, writeToFile, maxComments
#'
#' \code{twitter}: searchTerm, numTweets, verbose, writeToFile, language
#'
#' \code{instagram}: credential, tag, n, lat, lng, distance, folder, mindate,
#' maxdate, verbose, sleep, writeToFile, waitForRateLimit
#'
#' \code{instagram} with \code{ego} = TRUE: username, userid, verbose,
#' degreeEgoNet, waitForRateLimit, getFollows
#' @return A data.frame object of class \code{dataSource.*} that can be used
#' with \code{Create}.
#' @author Chung-hong Chan <chainsawtiney@@gmail.com>
#' @seealso \code{CollectDataFromFacebook},
#' \code{CollectDataFromInstagram},
#' \code{CollectDataFromYoutube}, \code{CollectDatFromTwitter},
#' \code{CollectEgoInstagram}
#' @examples
#'
#' \dontrun{
#' require(magrittr)
#' ## Instagram ego network example
#' myAppID <- "123456789098765"
#' myAppSecret <- "abc123abc123abc123abc123abc123ab"
#' myUsernames <- c("senjohnmccain","obama")
#'
#' Authenticate("instagram",
#' appID = myAappId,
#' appSecret = myAppSecret) %>% Collect(ego = TRUE,
#' username = myUsernames) %>% Create
#'
#' ## YouTube actor network example
#' my_apiKeyYoutube <- "314159265358979qwerty"
#' videoIDs <- c("W2GZFeYGU3s","mL27TAJGlWc")
#'
#' Authenticate("youtube",
#' apiKey = my_apiKeyYoutube) %>% Collect(videoIDs = videoIDs) %>% Create('actor')
#' }
#' @export
Collect <- function(credential, ego = FALSE, ...) {
    if (ego) {
        collector <- switch(credential$socialmedia,
                            instagram = instagramEgo,
                            stop("Unsupported socialmedia")
                            )
    } else {
        collector <- switch(credential$socialmedia,
                            facebook = facebookCollector,
                            youtube = youtubeCollector,
                            twitter = twitterCollector,
                            instagram = instagramCollector,
                            stop("Unsupported socialmedia")
                            )
    }
    return(collector(credential, ...))
}

### *collector functions should not be exported. It is just a bunch of helper functions to bridge the CollectDataFrom* functions with Collect(), but with credential obj as the first argument

youtubeCollector <-
    function(credential, videoIDs, verbose, writeToFile, maxComments) {
        return(CollectDataYoutube(videoIDs, apiKeyYoutube = credential$auth, verbose, writeToFile, maxComments))
}

facebookCollector <-
    function(credential,pageName,rangeFrom,rangeTo,verbose,n,writeToFile) {
        return(CollectDataFacebook(pageName,rangeFrom,rangeTo,verbose,n,writeToFile, credential))
}

twitterCollector <- function(credential, searchTerm, numTweets, verbose, writeToFile, language, ...) {
    return(CollectDataTwitter(searchTerm, numTweets, verbose, writeToFile, language, ...)) # credential means nothing to twitteR
}

instagramCollector <- function(credential, tag, n, lat, lng, distance, folder, mindate, maxdate, verbose, sleep, writeToFile, waitForRateLimit) {
    return(CollectDataInstagram(tag, n, lat, lng, distance, folder, mindate, maxdate, verbose, sleep, writeToFile, waitForRateLimit, credential))
}

instagramEgo <- function(credential, username, userid, verbose, degreeEgoNet, waitForRateLimit, getFollows) {
    return(CollectEgoInstagram(username, userid, verbose, degreeEgoNet, waitForRateLimit, getFollows, credential))
}
