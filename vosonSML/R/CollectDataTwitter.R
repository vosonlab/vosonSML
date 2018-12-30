#' Note: this function is DEPRECATED. Please use the \code{\link{Collect}} function.
#'
#' Collect data from Twitter for generating different types of networks
#'
#' This function collects data from Twitter based on hashtags or search terms, and structures the data into a data 
#' frame of class \code{dataSource.twitter}, ready for creating networks for further analysis.
#'
#' \code{CollectDataTwitter} collects public 'tweets' from Twitter using the Twitter API.
#'
#' The function then finds and maps the relationships of entities of interest in the data (e.g. users, terms, hashtags)
#' , and structures these relationships into a data frame format suitable for creating unimodal networks 
#' (\code{CreateActorNetwork}), bimodal networks (\code{CreateBimodalNetwork}), and semantic networks 
#' (\code{CreateSemanticNetwork}).
#'
#' The maximum number of tweets for a single call of \code{CollectDataTwitter} is 1500.
#'
#' Language support is available, using the \code{language} parameter. The user can restrict tweets returned to a 
#' particular language, using the ISO 639-1 code. For example, restricting to English would use \code{language="en"}. 
#' The full list of codes is available here: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes.
#'
#' A variety of query operators are available through the Twitter API. For example, "love OR hate" returns any tweets 
#' containing either term (or both). For more information see the Twitter API documentation (under the heading
#' 'Query Operators'): https://dev.twitter.com/rest/public/search
#'
#' @param authToken Twitter oauth token created by rtweet.
#' @param searchTerm Character string. Specifies a search term or phrase (e.g. "Australian politics") or hashtag (e.g. 
#' "#auspol"). Many query operators are available - see the Twitter documentation for more information: 
#' https://dev.twitter.com/rest/public/search
#' @param searchType Character string. Returns filtered tweets as per search type \code{recent}, \code{mixed} or 
#' \code{popular}. Default type is \code{recent}.
#' @param numTweets Numeric. Specifies how many tweets to be collected. Defaults is \code{100}.
#' @param includeRetweets Logical. Specifies if the search should filter out retweets. Defaults is \code{TRUE}.
#' @param retryOnRateLimit Logical. Default is \code{FALSE}.
#' @param writeToFile Logical. If \code{TRUE} then the data is saved to file in current working directory (RDS format), 
#' with filename denoting current system time and \code{searchTerm}. Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE} then this function will output runtime information to the console as it 
#' computes. Useful diagnostic tool for long computations. Default is \code{FALSE}.
#' @param ... Additional parameters to pass to the rtweet \code{search_tweets} function.
#' 
#' @return A data frame object of class \code{dataSource.twitter} that can be used for creating unimodal networks 
#' (\code{CreateActorNetwork}), bimodal networks (\code{CreateBimodalNetwork}), and semantic networks 
#' (\code{CreateSemanticNetwork}).
#' 
#' @note Supported network types: \code{actor}, \code{bimodal}, \code{semantic}
#' 
#' Data generated using this function is *not* suitable for dynamic networks.
#' 
#' @seealso \code{Collect}
#' @keywords collect twitter
#' 
CollectDataTwitter <- function(authToken, searchTerm, searchType, numTweets, includeRetweets, 
                               retryOnRateLimit, writeToFile, verbose, ...) {
 
  if (missing(authToken)) {
    cat("\nOAuth token missing. Please use the Authenticate function to create and supply a token.\n")
    return(NA)
  }
  
  if (missing(numTweets)) {
    numTweets <- 100
  }
 
  if (missing(includeRetweets)) {
    includeRetweets <- TRUE
  }
  
  if (missing(retryOnRateLimit)) {
    retryOnRateLimit <- FALSE
  }
  
  if (missing(writeToFile)) {
    writeToFile <- FALSE
  }
 
  if (missing(verbose)) {
    verbose <- FALSE
  }
  
  rtlimit <- rtweet::rate_limit(authToken, "search/tweets")
  remaining <- rtlimit[["remaining"]] * 100
  if (retryOnRateLimit == TRUE & numTweets < remaining) {
    cat(paste0("Requested ", numTweets, " tweets of ", remaining, " in this rate limit.\n"))
    cat("Less tweets requested than remaining limit retryOnRateLimit set to FALSE.\n")
    retryOnRateLimit <- FALSE
  }
  
  collect_parameters <- list()
  collect_parameters[['token']] <- authToken
  
  collect_parameters['q'] <- searchTerm
  collect_parameters['type'] <- searchType
  collect_parameters['n'] <- numTweets
  collect_parameters['include_rts'] <- includeRetweets
  collect_parameters['retryonratelimit'] <- retryOnRateLimit
  collect_parameters['verbose'] <- verbose
  
  dots <- substitute(...())
  collect_parameters[['...']] <- dots
  
  tweets_df <- do.call(rtweet::search_tweets, collect_parameters)
  
  # flatten hashtags
  # tweets_df$hashtags <- paste0(unlist(tweets_df$hashtags), collapse = ',')
  # all of the lists in the data make csv output less than ideal
  
  if (isTrueValue(writeToFile)) {
    writeOutputFile(tweets_df, "rds", "TwitterData")
  }
  
  class(tweets_df) <- append(class(tweets_df), c("dataSource", "twitter"))
  
  return(tweets_df)
}
