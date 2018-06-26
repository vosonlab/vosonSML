#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Collect} function
#'
#' Collect data from Twitter for generating different types of networks
#'
#' This function collects data from Twitter based on hashtags or search terms,
#' and structures the data into a data frame of class
#' \code{dataSource.twitter}, ready for creating networks for further analysis.
#'
#' \code{CollectDataTwitter} collects public 'tweets' from Twitter using the
#' Twitter API.
#'
#' The function then finds and maps the relationships of entities of interest
#' in the data (e.g. users, terms, hashtags), and structures these
#' relationships into a data frame format suitable for creating unimodal
#' networks (\code{CreateActorNetwork}), bimodal networks
#' (\code{CreateBimodalNetwork}), and semantic networks
#' (\code{CreateSemanticNetwork}).
#'
#' The maximum number of tweets for a single call of \code{CollectDataTwitter}
#' is 1500.
#'
#' Language support is available, using the \code{language} argument. The user
#' can restrict tweets returned to a particular language, using the ISO 639-1
#' code. For example, restricting to English would use \code{language="en"}.
#' The full list of codes is available here:
#' https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes.
#'
#' A variety of query operators are available through the Twitter API. For
#' example, "love OR hate" returns any tweets containing either term (or both).
#' For more information see the Twitter API documentation (under the heading
#' 'Query Operators'): https://dev.twitter.com/rest/public/search
#'
#' @param searchTerm character string, specifying a search term or phrase (e.g.
#' "Australian politics") or hashtag (e.g. "#auspol"). Many query operators are
#' available - see the Twitter documentation for more information:
#' https://dev.twitter.com/rest/public/search
#' @param numTweets numeric integer, specifying how many tweets to be
#' collected. Defaults to 1500. Maximum tweets for a single call of this
#' function is 1500.
#' @param verbose logical. If \code{TRUE} then this function will output
#' runtime information to the console as it computes. Useful diagnostic tool
#' for long computations. Default is \code{FALSE}.
#' @param writeToFile logical. If \code{TRUE} then the data is saved to file in
#' current working directory (CSV format), with filename denoting current
#' system time and \code{searchTerm}. Default is \code{FALSE}.
#' @param language character string, restricting tweets to the given language,
#' given by an ISO 639-1 code. For example, "en" restricts to English tweets.
#' Defaults to NULL.
#' @param since If not NULL, restricts tweets to those since the given date. Date is to be formatted
#' as YYYY-MM-DD (this is a wrapper to the searchTwitter function in the twitteR package).
#' @param until If not NULL, restricts tweets to those up until the given date. Date is to be formatted
#' as YYYY-MM-DD (this is a wrapper to the searchTwitter function in the twitteR package).
#' @param locale If not NULL, will set the locale for the search. As of 03/06/11 only ja is effective,
#' as per the Twitter API (this is a wrapper to the searchTwitter function in the twitteR package).
#' @param geocode If not NULL, returns tweets by users located within a given radius of the given
#' latitude/longitude. (this is a wrapper to the searchTwitter function in the twitteR package).
#' @param sinceID If not NULL, returns tweets with IDs greater (ie newer) than the specified ID
#' (this is a wrapper to the searchTwitter function in the twitteR package).
#' @param maxID If not NULL, returns tweets with IDs smaller (ie older) than the specified ID
#' (this is a wrapper to the searchTwitter function in the twitteR package).
#' @param resultType If not NULL, returns filtered tweets as per value. See details for allowed values.
#' (this is a wrapper to the searchTwitter function in the twitteR package).
#' @param retryOnRateLimit If non-zero the search command will block retry up to X times if the rate limit
#' is experienced. This might lead to a much longer run time but the task will
#' eventually complete if the retry count is high enough (this is a wrapper to the searchTwitter
#' function in the twitteR package).
#' @return A data frame object of class \code{dataSource.twitter} that can be
#' used for creating unimodal networks (\code{CreateActorNetwork}), bimodal
#' networks (\code{CreateBimodalNetwork}), and semantic networks
#' (\code{CreateSemanticNetwork}).
#' @note Data generated using this function is *not* suitable for dynamic
#' networks. Dynamic Twitter networks are not currently implemented in the
#' vosonSML package. This will be implemented in a future release.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithTwitterAPI} must be run first or no data will
#' be collected.
#' @keywords twitter data mining SNA
#' @examples
#'
#' \dontrun{
#'   # Firstly specify your API credentials
#'   my_api_key <- "1234567890qwerty"
#'   my_api_secret <- "1234567890qwerty"
#'   my_access_token <- "1234567890qwerty"
#'   my_access_token_secret <- "1234567890qwerty"
#'
#'   # Authenticate with the Twitter API using \code{AuthenticateWithTwitterAPI}
#'   AuthenticateWithTwitterAPI(api_key=my_api_key, api_secret=my_api_secret,
#'     access_token=my_access_token, access_token_secret=my_access_token_secret)
#'
#'   # Collect tweets data using \code{myTwitterData}
#'   myTwitterData <- CollectDataTwitter(searchTerm="#auspol",
#'     numTweets=150,writeToFile=FALSE,verbose=FALSE)
#'
#'   # Create an 'actor' network using \code{CreateActorNetwork}
#'   g_actor_twitter <- CreateActorNetwork(myTwitterData)
#'
#'   # Create a 'bimodal' network using \code{CreateBimodalNetwork}
#'   g_bimodal_twitter <- CreateBimodalNetwork(myTwitterData)
#'
#'   # Create a 'semantic' network using \code{CreateSemanticNetwork}
#'   g_semantic_twitter <- CreateSemanticNetwork(myTwitterData)
#'   }
#' @export
CollectDataTwitter <-
function(searchTerm, numTweets, verbose, writeToFile, language, since, until,
  locale, geocode, sinceID, maxID, resultType, retryOnRateLimit) {

# cat(paste("DEBUG - numTweets is set to:", numTweets)) # DEBUG

  # handle the arguments

  if (missing(verbose)) {
    verbose <- TRUE # default to verbose
  }

  if (missing(language)) {
    language <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE) {
    verbose <- TRUE
  }
  else {verbose <- FALSE}

  if (missing(numTweets)) {
    numTweets <- 1500 # default to 1500 max tweets
  }

  # Ensure that argument `pageName` has been specified by user.

  if (missing(searchTerm)) {
    cat("Error. Argument `searchTerm` is missing.\nPlease specify a search term or hashtag to collect data from.\n")
    return(NA)
  }

  if (missing(since)) {
    since <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(until)) {
    until <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(locale)) {
    locale <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(geocode)) {
    geocode <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(sinceID)) {
    sinceID <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(maxID)) {
    maxID <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(resultType)) {
    resultType <- NULL # default to NULL (as per 'twitteR' package default)
  }

  if (missing(retryOnRateLimit)) {
    retryOnRateLimit <- 0 # default to NULL (as per 'twitteR' package default)
  }

  # Start data collection
if (verbose) {
  cat(paste("Now retrieving data based on search term: ",searchTerm,"\n",sep=""))
  flush.console()
}
  # Collecting tweets based on hashtag / keyword

  tweetsData <- searchTwitter(searchTerm, n=numTweets, lang=language, since=since, until=until,
    locale=locale, geocode=geocode, sinceID=sinceID, maxID=maxID, resultType=resultType, retryOnRateLimit=retryOnRateLimit) #1500 is max

  # Convert this data into a dataframe object, for ease of use
  if (verbose) {
    cat("Done\n")  ### DEBUG
    flush.console()
    cat("Cleaning and sorting the data...\n")
  }
  df <- twListToDF(tweetsData) # a better way

  # rename metadata
    names.twitteR <- c("screenName", "created") # change from
    names.api <- c("screen_name", "created_at") # change to
    for(name in names.twitteR) {
      names(df)[which(names(df)==name)] <- names.api[which(names.twitteR==name)]
    }
    df$from_user <- df$screen_name

  # removing odd characters
  df <- RemoveOddChars(df)

  # extract user info and add to df
  df <- ExtractUserInfo(df)

  # extract HASHTAG info and add to df
  df <- ExtractHashtagInfo(df)
  if (verbose) {
    cat("Done\n")  ### DEBUG
    flush.console()
  }
  ################################################

  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    tweetsDataDF <- twListToDF(tweetsData) # we just want the original tweets data
    currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
    currTime <- gsub(":","_",currTime)
    write.csv(tweetsDataDF,paste0(currTime,"_",searchTerm,"_TwitterData.csv"))
    cat("Twitter data was written to current working directory, with filename:\n")
    cat(paste0(currTime,"_",searchTerm,"_TwitterData.csv"))
  }

  class(df) <- append(class(df),c("dataSource","twitter"))

  cat("\n")

  return(df)

  ################################################

}
