#' Collect reddit data.
#'
#' Uses reddit thread url's to collect user and comment data.
#' 
#' @param oauth2_token A Token object returned from reddit oauth2 authentication.
#' @param thread_urls A vector of reddit thread url's to collect data from.
#' @param ua Character string containing a user-agent string to use in the get request.
#' @param write_to_file Boolean. If the data should be written to file. 
#' @return A data frame object of class \code{dataSource.reddit} that can be used for creating unimodal 
#' networks (\code{CreateActorNetwork}).
#' 
CollectDataReddit <- function(oauth2_token, thread_urls, ua, write_to_file) {

  if (missing(oauth2_token)) {
    cat("Error. Argument `oauth2_token` is missing.\nPlease provide an authentication token.\n")
    return(NA)
  }
  
  if (missing(thread_urls)) {
    cat("Error. Argument `thread_urls` is missing.\nPlease provide a reddit thread url.\n")
    return(NA)
  }
  
  if (missing(ua)) {
    ua <- "httr oauth"
  }
  
  if (missing(write_to_file)) {
    write_to_file <- FALSE
  }

  # make the get request for the reddit thread url
  reddit_response <- GET(thread_urls[1], user_agent(ua), config(token = oauth2_token))
  
  # parse the json results
  resp_parsed <- content(reddit_response, "parsed")
  
  if (write_to_file == TRUE) {
    current_time <- format(Sys.time(), "%b_%d_%H_%M_%S_%Y")
    save_file_name <- paste0(current_time, "_RedditData.rds")
    saveRDS(resp_parsed, file = save_file_name)
    
    cat(paste0("Reddit collection data was written to working directory, with filename:\n", save_file_name, "\n"))
  }
  
  # create a dataframe
  thread_meta <- resp_parsed[[1]]$data$children[[1]]$data
  thread_data <- resp_parsed[[2]]$data$children
  
  thread_structure <- unlist(lapply(1:length(thread_data), 
                                    function(x) { get_thread_structure(thread_data[[x]], x) }))
  
  thread_df <- data.frame(
    id        = NA,
    link_id   = unlist(lapply(thread_data, function(x) { get_node_attribute(x, "link_id") })),
    parent_id = unlist(lapply(thread_data, function(x) { get_node_attribute(x, "parent_id") })),
    structure = gsub("FALSE ", "", thread_structure[!grepl("TRUE", thread_structure)]),
    comm_date = reddit_utc2datetime(unlist(lapply(thread_data, 
                                                  function(x){ get_node_attribute(x, "created_utc") }))),
    user      = unlist(lapply(thread_data, function(x) { get_node_attribute(x, "author") })),
    post_score = unlist(lapply(thread_data, function(x) { get_node_attribute(x, "score") })),
    comment    = unlist(lapply(thread_data, function(x) { get_node_attribute(x, "body") })),
    author     = thread_meta$author,
    title      = thread_meta$title,
    
    stringsAsFactors = FALSE
  )
  
  thread_df$id = 1:nrow(thread_df)
  
  if (write_to_file == TRUE) {
    current_time <- format(Sys.time(), "%b_%d_%H_%M_%S_%Y")
    save_file_name <- paste0(current_time, "_RedditData.csv")
    write.csv(thread_df, save_file_name)
    
    cat(paste0("Reddit dataframe CSV was written to working directory, with filename:\n", save_file_name, "\n"))
  }
  
  class(thread_df) <- append(class(thread_df), c("dataSource", "reddit"))
  
  cat("\n")
  
  return(thread_df)
}

reddit_utc2datetime <- function(utc_ts) {
  as.POSIXct(as.numeric(utc_ts), origin = "1970-01-01", tz = "GMT")
}

# https://github.com/cran/RedditExtractoR/blob/master/R/reddit_content.R
# GetAttribute by @ivan-rivera
get_node_attribute <- function(node, field){
  node_attribute <- node$data[[field]]
  replies <- node$data$replies
  
  reply_nodes = if (is.list(replies)) {
    replies$data$children
  } else {
    NULL
  }
  
  return(list(node_attribute, lapply(reply_nodes, function(x) { get_node_attribute(x, field) })))
}

# https://github.com/cran/RedditExtractoR/blob/master/R/reddit_content.R
# get.structure by @ivan-rivera
get_thread_structure <- function(node, depth = 0) {
  if(is.null(node)) {
    return(list())
  }
  
  filter <- is.null(node$data$author)
  replies <- node$data$replies
  reply_nodes <- if (is.list(replies)) {
    replies$data$children
  } else {
    NULL
  }
  
  return(list(paste0(filter, " ", depth),
              lapply(1:length(reply_nodes), function(x) get_thread_structure(reply_nodes[[x]], paste0(depth, "_", x)))))
}