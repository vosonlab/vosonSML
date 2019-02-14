#' @title Collect comments data from reddit threads
#'
#' @description Collects comments made by users on one or more specified subreddit conversation threads and structures 
#' the data into a dataframe with the class names \code{"datasource"} and \code{"reddit"}.
#' 
#' @note The reddit web endpoint used for collection has maximum limit of 500 comments per thread url.
#' 
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"reddit"}.
#' @param threadUrls Character vector. Reddit thread urls to collect data from.
#' @param waitTime Numeric integer. Time in seconds to wait in-between url collection requests.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return A \code{data.frame} object with class names \code{"datasource"} and \code{"reddit"}.
#' 
#' @examples
#' \dontrun{
#' # subreddit url to collect threads from
#' threadUrls <- c("https://www.reddit.com/r/xxxxxx/comments/xxxxxx/x_xxxx_xxxxxxxxx/")
#' 
#' redditData <- redditAuth %>%
#'   Collect(threadUrls = threadUrls, waitTime = 3, writeToFile = TRUE)
#' }
#' 
#' @export
Collect.reddit <- function(credential, threadUrls, waitTime = 5, writeToFile = FALSE, ...) {
  
  if (missing(threadUrls) || !is.vector(threadUrls) || length(threadUrls) < 1) {
    stop("Please provide a vector of one or more reddit thread urls.", call. = FALSE)
  }

  cat("Collecting thread data for reddit urls...\n")
  
  # reddit_content uses a progress bar that defaults to option width
  # set to be much smaller than page
  save_width <- getOption("width")
  # progress_width <- save_width - 40
  # if (progress_width >= 20) {
  #   options("width" = progress_width)
  # }
  options("width" = 60)
  
  # make the get request for the reddit thread url
  threads_df <- tryCatch({
    RedditExtractoR::reddit_content(threadUrls, waitTime)
  }, error = function(e) {
    stop(gsub("^Error:\\s", "", paste0(e)), call. = FALSE)
  }, finally = {
    # reset width
    options("width" = save_width)
  })
  
  # add thread id to df, extracted from url
  threads_df$thread_id <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\2", 
                               threads_df$URL, ignore.case = TRUE, perl = TRUE)
  
  if (writeToFile) { writeOutputFile(threads_df, "csv", "RedditData") }
  
  cat("Done.\n")
  flush.console()
  
  class(threads_df) <- append(class(threads_df), c("datasource", "reddit"))
  
  return(threads_df)
}