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
  
  if (!is.null(threads_df) & nrow(threads_df) > 0) {
    # add thread id to df, extracted from url
    threads_df$thread_id <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\2", 
                                 threads_df$URL, ignore.case = TRUE, perl = TRUE)
    
    # summary
    cat(paste0("Collected ", nrow(threads_df), " total comments.\n"))
    
    results_df <- threads_df %>% 
      dplyr::group_by(thread_id) %>%
      dplyr::summarise(title = paste0(unique(title), collapse = ","), 
                       subreddit = paste0(unique(subreddit), collapse = ","), 
                       count = dplyr::n()) %>%
      dplyr::ungroup()
    
    results_df$title <- ifelse(nchar(results_df$title) > 32, paste0(strtrim(results_df$title, 32), "..."), 
                               results_df$title)
    printResultTable(results_df)
    
    if (writeToFile) { writeOutputFile(threads_df, "csv", "RedditData") }
  } else {
    cat(paste0("No comments were collected.\n"))
  }
  
  cat("Done.\n")
  flush.console()
  
  class(threads_df) <- append(class(threads_df), c("datasource", "reddit"))
  
  return(threads_df)
}
