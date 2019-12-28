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
  
  if (!requireNamespace("RedditExtractoR", quietly = TRUE)) {
    stop("Please install the RedditExtractoR package before calling Collect.", call. = FALSE)
  }
  
  cat("Collecting comment threads for reddit urls...\n")
  
  if (missing(threadUrls) || !is.vector(threadUrls) || length(threadUrls) < 1) {
    stop("Please provide a vector of one or more reddit thread urls.", call. = FALSE)
  }

  locale_list <- c("LC_COLLATE", "LC_CTYPE", "LC_MONETARY", "LC_NUMERIC", "LC_TIME") # LC_ALL
  saved_locale <- setNames(lapply(locale_list, Sys.getlocale), locale_list)
  on.exit({ lapply(names(saved_locale), function(x) { Sys.setlocale(x, unlist(saved_locale[[x]])) }) })
  Sys.setlocale("LC_ALL", "C")
  
  threads_df <- NULL
  
  # cat(paste0("encoding: ", getOption("encoding"), "\n"))
  
  # make the get request for the reddit thread url
  tryCatch({
    capture.output(threads_df <- RedditExtractoR::reddit_content(threadUrls, waitTime), type = c("output"))
  }, error = function(e) {
    stop(gsub("^Error:\\s", "", paste0(e)), call. = FALSE)
  # }, warning = function(w) {
  #   
  }, finally = { 
    threads_df <- as_tibble(threads_df)
  })
  
  if (!is.null(threads_df)) {
    if (nrow(threads_df) > 0) {
      # add thread id to df, extracted from url
      threads_df$thread_id <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\2", 
                                   threads_df$URL, ignore.case = TRUE, perl = TRUE, useBytes = TRUE)
      
      cat("HTML decoding comments.\n")
      threads_df$comment <- textutils::HTMLdecode(threads_df$comment)
      
      # summary
      results_df <- threads_df %>% 
        dplyr::group_by(.data$thread_id) %>%
        dplyr::summarise(title = paste0(unique(.data$title), collapse = ","), 
                         subreddit = paste0(unique(.data$subreddit), collapse = ","), 
                         count = dplyr::n()) %>%
        dplyr::ungroup()
      
      results_df$title <- ifelse(nchar(results_df$title) > 42, paste0(strtrim(results_df$title, 42), "..."), 
                                 results_df$title)
      printResultTable(results_df)
      cat(paste0("Collected ", nrow(threads_df), " total comments.\n"))
      
      if (writeToFile) { writeOutputFile(threads_df, "csv", "RedditData") }
    } else {
      cat(paste0("No comments were collected.\n"))
    }
  } else {
    cat(paste0("Collection dataframe is null.\n"))
  }
  
  class(threads_df) <- append(class(threads_df), c("datasource", "reddit"))
  cat("Done.\n")
  
  return(threads_df)
}
