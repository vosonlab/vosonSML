#' Collect data from social media for generating networks
#'
#' This function collects data from social media APIs, and structures the data into a data frame of class
#' \code{dataSource.*}, ready for creating networks for further analysis. \code{Collect} is the second step of the
#' \code{Authenticate}, \code{Collect}, \code{Create} workflow. This function is a convenient UI wrapper to the core
#' CollectDataFrom* family of functions.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate}.
#' @param ... Additional parameters for data collection by appropriate to credential \code{socialmedia} type.
#' \describe{
#'   \item{twitter:}{\code{authToken, searchTerm, [searchType, numTweets, includeRetweets, retryOnRateLimit,}\cr
#'                   \code{writeToFile, verbose, ...]}}
#'   \item{youtube:}{\code{videoIDs, apiKeyYoutube, [verbose, writeToFile, maxComments]}}
#'   \item{reddit:}{\code{threadUrls, [waitTime, writeToFile]}}
#' }
#'
#' @return A data.frame object of class \code{dataSource.*} that can be used with \code{Create}.
#'
#' @seealso \code{Authenticate}, \code{Create}
#' @keywords collect twitter youtube reddit
#'
#' @export
Collect <- function(credential, ...) {
  collector <- switch(credential$socialmedia,
                      twitter = twitterCollector,
                      youtube = youtubeCollector,
                      reddit = redditCollector,
                      stop("Collect called with unsupported social media type.", call. = FALSE))

  return(collector(credential, ...))
}

twitterCollector <- function(credential, ...) {
  return(CollectDataTwitter(authToken = credential$auth, ...))
}

youtubeCollector <- function(credential, ...) {
  return(CollectDataYoutube(apiKey = credential$auth, ...))
}

redditCollector <- function(credential, ...) {
  return(CollectDataReddit(...))
}
