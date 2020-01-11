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
  
  # if (!requireNamespace("RedditExtractoR", quietly = TRUE)) {
  #   stop("Please install the RedditExtractoR package before calling Collect.", call. = FALSE)
  # }
  
  cat("Collecting comment threads for reddit urls...\n")
  
  if (missing(threadUrls) || !is.vector(threadUrls) || length(threadUrls) < 1) {
    stop("Please provide a vector of one or more reddit thread urls.", call. = FALSE)
  }

  locale_list <- c("LC_COLLATE", "LC_CTYPE", "LC_MONETARY", "LC_NUMERIC", "LC_TIME") # LC_ALL
  saved_locale <- setNames(lapply(locale_list, Sys.getlocale), locale_list)
  on.exit({ lapply(names(saved_locale), function(x) { Sys.setlocale(x, unlist(saved_locale[[x]])) }) })
  Sys.setlocale("LC_ALL", "C")
  
  threads_df <- NULL
  
  tryCatch({
    threads_df <- reddit_build_df(threadUrls, waitTime = waitTime)
  }, error = function(e) {
    stop(gsub("^Error:\\s", "", paste0(e)), call. = FALSE)
  }, finally = { 
    threads_df <- as_tibble(threads_df)
  })
  
  if (!is.null(threads_df)) {
    if (nrow(threads_df) > 0) {
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

reddit_build_df <- function(threadUrls, waitTime = 2) {
  threads <- lapply(threadUrls, function(x) {
    json <- reddit_data(x, waitTime = waitTime)
    df <- reddit_content_plus(json, x)
    
    # process continue threads
    plus <- filter(df, grepl("Listing:", .data$comm_id))
    while (nrow(plus) > 0) {
      row <- 1
      row_id <- as.numeric(plus[row, "id"])
      depth <- as.numeric(gsub(".*_(\\d)_\\d$", "\\1", plus[row, "structure"]))
      struct <- gsub("_\\d_\\d$", "", plus[row, "structure"])
      cont_thread_id <- gsub("Listing:t1_", "", plus[row, "comm_id"])
      
      df <- mutate(df, rm = ifelse((.data$comm_id == cont_thread_id | .data$comm_id == plus[row, "comm_id"]), TRUE, .data$rm))
      cat(paste0("Continue thread: ", cont_thread_id))
      
      data <- reddit_data(paste0(x, cont_thread_id))
      thread_df <- reddit_content_plus(data, x, depth = depth)
      
      if (nrow(thread_df)) {
        cat(paste0(" (comments: ", nrow(thread_df), ")\n"))
        thread_df <- thread_df %>% mutate(structure = paste0(struct, "_", .data$structure))
        if (row_id == 1) {
          df <- rbind(thread_df, df)
        } else {
          pre_df <- rbind(df[1:row_id-1, ], thread_df)
          df <- rbind(pre_df, df[row_id:nrow(df), ])
        }
      } else {
        cat(paste0(" (no comments found)\n"))
      }
      
      plus <- plus[-row, ]
      plus <- filter(df, grepl("Listing:", .data$comm_id), .data$rm == FALSE)
      Sys.sleep(sample(waitTime:10, 1))
    
    } # end while
    
    df$thread_id <- gsub("^(.*)?/comments/([0-9A-Za-z]{6})?/.*?(/)?$", "\\2", 
                         df$url, ignore.case = TRUE, perl = TRUE, useBytes = TRUE)
    df <- df %>% filter(.data$rm == FALSE) 
    df <- df %>% arrange(.data$thread_id, .data$id) %>% mutate(id = 1:nrow(df))
  })
  
  threads_df <- bind_rows(threads)
}

# original author of method @ivan-rivera
reddit_data <- function(url, waitTime = 2) {
  if (is.null(url) | length(url) == 0 | !is.character(url)) { stop("invalid url parameter") }
  
  if (!grepl("^https?://(.*)", url)) url = paste0("https://www.",gsub("^.*(reddit\\..*$)","\\1",url))
  if (!grepl("\\?ref=search_posts$",url)) url = paste0(gsub("/$","",url),"/?ref=search_posts")
  
  req_url = paste0(gsub("\\?ref=search_posts$","",url),".json?limit=500&raw_json=1&enc=utf8")
  
  raw_data = tryCatch(RJSONIO::fromJSON(readLines(req_url, warn = FALSE)), error = function(e) NULL)
  
  if (is.null(raw_data)) {
    Sys.sleep(sample(waitTime:10, 1))
    raw_data = tryCatch(RJSONIO::fromJSON(readLines(req_url, warn = FALSE)), error = function(e) NULL)
  }
  
  raw_data
}

# original author of method @ivan-rivera
reddit_values_list  = function(node, feature) {
  attr <- node$data[[feature]]
  if (is.null(attr)) {
    attr <- NA
  }
  if (feature == "id") {
    if (attr == "_") {
      attr <- paste0("Listing:", node$data$parent_id)
    }
  }
  replies <- node$data$replies
  reply_nodes <- NULL
  if (is.list(replies)) {
    reply_nodes <- replies$data$children
  }
  
  attrs <- list(attr, 
    lapply(reply_nodes, function(x) { 
      reddit_values_list(x, feature) 
    }))
}

# original author of method @ivan-rivera
reddit_struct_list = function(node, depth = 0) {
  if (is.null(node)) { return(list()) }
  
  reply_nodes <- NULL
  replies <- node$data$replies
  if (is.list(replies)) {
    reply_nodes <- replies$data$children
  }
  
  structures <- list(depth,
   lapply(1:length(reply_nodes), function(x) {
     reddit_struct_list(reply_nodes[[x]], paste0(depth, "_", x))
   }))
}

# original author of method @ivan-rivera
reddit_content_plus <- function(raw_data, req_url, depth = 0) {
  
  data_extract <- data.frame(
    id = numeric(),             structure = character(),
    post_date = character(),    post_date_unix = numeric(),
    comm_id = character(),      comm_date = character(),
    comm_date_unix = numeric(), num_comments = numeric(),
    subreddit = character(),    upvote_prop = numeric(),
    post_score = numeric(),     author = character(),
    user = character(),         comment_score = numeric(),
    controversiality = numeric(), comment = character(),
    title = character(),          post_text = character(),
    link = character(),           domain = character(),
    url = character(),            rm = logical())
  
  meta_node <- raw_data[[1]]$data$children[[1]]$data
  main_node <- raw_data[[2]]$data$children
  
  if (min(length(meta_node), length(main_node)) > 0) {
    
    structures_list <- unlist(lapply(1:length(main_node), function(x) {
      reddit_struct_list(main_node[[x]], depth = ifelse(depth != 0, depth, x))
    }))
    
    data <- data.frame(
      id               = NA,
      structure        = structures_list,
      post_date        = as.character(
        as_datetime(as.numeric(meta_node$created_utc), tz = "UTC")
      ),
      post_date_unix   = as.numeric(meta_node$created_utc),
      comm_id          = unlist(lapply(main_node, function(x) { reddit_values_list(x, "id") })),      
      comm_date        = as.character(
        as_datetime(as.numeric(unlist(lapply(main_node, function(x) { reddit_values_list(x, "created_utc") }))), tz = "UTC")
      ),
      comm_date_unix   = as.numeric(unlist(lapply(main_node, function(x) { reddit_values_list(x, "created_utc") }))),                                            
      num_comments     = meta_node$num_comments,
      subreddit        = ifelse(is.null(meta_node$subreddit), "UNKNOWN", meta_node$subreddit),
      upvote_prop      = meta_node$upvote_ratio,
      post_score       = meta_node$score,
      author           = meta_node$author,
      user             = unlist(lapply(main_node, function(x) { reddit_values_list(x, "author") })),
      comment_score    = unlist(lapply(main_node, function(x) { reddit_values_list(x, "score") })),
      controversiality = unlist(lapply(main_node, function(x) { reddit_values_list(x, "controversiality") })),
      comment          = unlist(lapply(main_node, function(x) { reddit_values_list(x, "body") })),
      title            = meta_node$title,
      post_text        = meta_node$selftext,
      link             = meta_node$url,
      domain           = meta_node$domain,
      url              = req_url,
      rm               = FALSE,
      stringsAsFactors = FALSE)
    
    data$id <- 1:nrow(data)
    
    if (dim(data)[1] > 0 & dim(data)[2] > 0) {
      data_extract <- rbind(data, data_extract)
    } else {
      cat(paste0("No data: ", req_url, "\n"))
    }
  }
  
  data_extract
}
