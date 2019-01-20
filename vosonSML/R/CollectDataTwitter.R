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
#' The maximum number of tweets for a single call of \code{CollectDataTwitter} is 18000 as per the twitter standard
#' API rate limits. Only returns tweets for the last 6 to 9 days.
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
#' @seealso \code{Collect}
#' @keywords collect twitter
#' 
CollectDataTwitter <- function(authToken = NULL, searchTerm = "", searchType = "recent", numTweets = 100, 
                               includeRetweets = TRUE, retryOnRateLimit = FALSE, writeToFile = FALSE, 
                               verbose = FALSE, ...) {
 
  if (!("Token" %in% class(authToken))) { 
    stop("OAuth token missing. Please use the Authenticate function to create and supply a token.\n", 
         call. = FALSE)
  }
  
  searchTerm <- trimws(searchTerm)
  cat(paste0("Collecting tweets", ifelse(searchTerm == "", "", paste0(" for search term: ", searchTerm)), "...\n"))
  flush.console()
  
  rtlimit <- rtweet::rate_limit(authToken, "search/tweets")
  remaining <- rtlimit[["remaining"]] * 100
  if (retryOnRateLimit == TRUE & numTweets < remaining) {
    cat(paste0("Requested ", numTweets, " tweets of ", remaining, " in this rate limit.\n"))
    cat("Less tweets requested than remaining limit retryOnRateLimit set to FALSE.\n")
    retryOnRateLimit <- FALSE
  }
  
  search_params <- list()
  search_params[['token']] <- authToken
  
  search_params['q'] <- searchTerm
  search_params['type'] <- searchType
  search_params['n'] <- numTweets
  search_params['include_rts'] <- includeRetweets
  search_params['retryonratelimit'] <- retryOnRateLimit
  search_params['verbose'] <- verbose
  
  # additional twitter api params
  dots <- substitute(...())
  search_params[['...']] <- dots
  
  tweets_df <- do.call(rtweet::search_tweets, search_params)
  
  cat(paste0("Collected ", nrow(tweets_df), " tweets.\n"))
  
  # rds chosen over csv to avoid flattening lists in the data
  if (writeToFile) { writeOutputFile(tweets_df, "rds", "TwitterData") }
  
  cat("Done.\n")
  flush.console()
  
  class(tweets_df) <- append(class(tweets_df), c("dataSource", "twitter"))
  
  return(tweets_df)
}
