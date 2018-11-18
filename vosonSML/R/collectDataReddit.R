#' Collect reddit data
#'
#' Uses RedditExtractoR::reddit_content to collect user and comment data for thread urls.
#' 
#' @param thread_urls Character string vector. Reddit thread url's to collect data from.
#' @param wait_time Numeric integer. Time in seconds to wait in-between url collection requests.
#' @param write_to_file Boolean. If the data should be written to file. 
#' 
#' @note The reddit API endpoint used for thread collection has maximum limit of 500 comments per thread url.
#' 
#' @return A data frame object of class dataSource.reddit that can be used for creating unimodal 
#' networks (CreateActorNetwork).
#' 
collectDataReddit <- function(thread_urls, wait_time = 5, write_to_file) {
  
  if (missing(thread_urls)) {
    cat("Error. Argument `thread_urls` is missing.\nPlease provide a reddit thread url.\n")
    return(NA)
  }

  if (!is.vector(thread_urls) || length(thread_urls) < 1) {
    cat("Error. Please provide a vector of one or more reddit thread urls.\n")
    return(NA)    
  }
  
  if (missing(write_to_file)) {
    write_to_file <- FALSE
  }

  cat("\nCollecting thread data for reddit urls:\n")
  
  # make the get request for the reddit thread url
  threads_df <- RedditExtractoR::reddit_content(thread_urls, wait_time)
  
  if (isTrueValue(write_to_file)) {
    writeOutputFile(threads_df, "csv", "RedditData")
  }
  
  class(threads_df) <- append(class(threads_df), c("dataSource", "reddit"))
  
  cat("\nDone!\n")
  
  return(threads_df)
}