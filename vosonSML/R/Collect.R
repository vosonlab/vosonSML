#' Collect data from social media for generating networks
#'
#' This function collects data from social media and structures it into a dataframe that can be used for creating 
#' networks for further analysis. \code{Collect} is the second step of the \code{Authenticate}, \code{Collect}, 
#' \code{Create} workflow.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate}.
#' @param ... Additional parameters for data collection appropriate to \code{socialmedia} type. Refer to usage for 
#' S3 methods for class \code{socialmedia}.
#'
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{FALSE}.
#' 
#' @return A data.frame object of class \code{dataSource} that can be used with \code{Create}.
#'
#' @examples
#' \dontrun{
#' ## twitter data collection
#' 
#' # search and collect 100 recent tweets for the hashtag #auspol
#' myTwitterData <- myTwitterAuth %>% 
#'   Collect(searchTerm = "#auspol", searchType = "recent", numTweets = 100, verbose = TRUE, 
#'           includeRetweets = FALSE, retryOnRateLimit = TRUE, writeToFile = TRUE)
#' 
#' ## youtube data collection
#' 
#' # create a list of youtube video ids to collect on
#' videoIDs <- GetYoutubeVideoIDs(c("https://www.youtube.com/watch?v=xXXxxxXx"))
#' 
#' # collect all comments for the youtube videos 
#' myYoutubeData <- myYoutubeAuth %>% 
#'   Collect(videoIDs = videoIDs, writeToFile = TRUE, verbose = FALSE)
#'   
#' ## reddit data collection
#' 
#' # reddit threads to collect on
#' threadUrls <- c("https://www.reddit.com/r/xxxxxx/comments/xXxXXx/x_xxxx_xxxxxxxxx/")
#' 
#' myRedditData <- myRedditAuth %>%
#'   Collect(threadUrls = threadUrls, waitTime = 3, writeToFile = TRUE)
#' }
#' 
#' @seealso \code{\link{Authenticate}}, \code{\link{Create}}
#' @keywords collect twitter youtube reddit
#'
#' @export
Collect <- function(credential, ...) {
  # searches the class list of credential for matching method
  UseMethod("Collect", credential)
}

# default function
#' @export
Collect.default <- function(credential, ...) {
  stop("Unknown social media type passed to collect.", call. = FALSE) 
}
