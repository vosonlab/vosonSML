#' Creates a reddit actor network from collected threads
#'
#' Uses RedditExtractoR::user_network to create an igraph directed actor network with comment ids as edge attribute.
#'
#' @param x a dataframe as vosonSML class object containing collected social network data
#' @param weightEdges logical. Combines and weights directed edges. Can't be used with includeTextData.
#' @param includeTextData logical. If the igraph network edges should include the comment text as attribute.
#' @param cleanText logical. If non-alphanumeric, non-punctuation, and non-space characters should be removed from the 
#' included text attribute data. Default is TRUE
#' @param writeToFile logical. If the igraph network graph should be written to file.
#' 
#' @return an igraph object of the actor network
#'
#' @note Can create three types of network graphs:
#' * Directed graph with subreddit, thread_ids and comment ids as edge attributes - default option
#' * Directed graph with weighted edges (without comment ids) - weightEdges = TRUE
#' * Directed graph with comment text included as edge attribute - includeTextData = TRUE
#' 
#' Comment ids as edge attributes in graphs refer to the Collect dataframe comment id not reddits comment id 
#' If "Forbidden control character 0x19 found in igraph_i_xml_escape, Invalid value" then set cleanText = TRUE
#' 
CreateActorNetwork.reddit <- function(x, weightEdges, includeTextData, cleanText, writeToFile) {
  
  if (missing(writeToFile) || writeToFile != TRUE) {
    writeToFile <- FALSE
  }

  if (missing(weightEdges) || weightEdges != TRUE) {
    weightEdges <- FALSE
  }
  
  # if weightEdges then includeTextData set FALSE
  if (missing(includeTextData) || includeTextData != TRUE || weightEdges == TRUE) {
    includeTextData <- FALSE
  }
  
  # default cleanText = TRUE as reddit comments often contain forbidden XML control characters
  if (missing(cleanText) || cleanText != FALSE) {
    cleanText <- TRUE
  } else {
    cleanText <- FALSE
  }
  
  if (includeTextData == FALSE) {
    cleanText <- FALSE
  }
  
  # append string to file name to indicate different graph types, only used if writeToFile = TRUE
  appendToName <- ""
  
  thread_df <- x
  
  # actor_network <- RedditExtractoR::user_network(thread_df, include_author = TRUE, agg = FALSE)
  
  # modified from RedditExtractoR::user_network to include the df comment id, subreddit and thread id as edge 
  # attributes to support post-processing. author of sender_receiver_df, node_df, and edge_df @ivan-rivera.
  include_author <- TRUE
  sender_receiver_df <-
    thread_df %>% 
    dplyr::select(.data$id, .data$subreddit, .data$thread_id, .data$structure, .data$user, .data$author, 
                  .data$comment) %>% 
    dplyr::rename("comment_id" = .data$id, "sender" = .data$user) %>%
    dplyr::mutate(response_to = ifelse(!grepl("_", .data$structure), "", gsub("_\\d+$", "", .data$structure))) %>%
    dplyr::left_join(thread_df %>% 
                     dplyr::select(.data$structure, .data$user) %>%
                     dplyr::rename("response_to" = .data$structure, "receiver" = .data$user),
                     by = "response_to") %>% 
    dplyr::mutate(receiver = dplyr::coalesce(.data$receiver, ifelse(include_author, .data$author, ""))) %>%
    dplyr::filter(.data$sender != .data$receiver, 
                  !(.data$sender %in% c("[deleted]", "")), 
                  !(.data$receiver %in% c("[deleted]", ""))) %>% 
    dplyr::mutate(count = 1) %>%
    dplyr::select(.data$sender, .data$receiver, .data$comment_id, .data$subreddit, .data$thread_id, .data$comment, 
                  .data$count)
  
  node_df <- data.frame(user = with(sender_receiver_df, {unique(c(sender, receiver))}), 
                        stringsAsFactors = FALSE) %>% 
             dplyr::mutate(id = as.integer(dplyr::row_number() - 1)) %>% 
             dplyr::select(.data$id, .data$user)
  
  edge_df <- sender_receiver_df %>% 
             dplyr::left_join(node_df %>% 
                              dplyr::rename("sender" = .data$user, "from" = .data$id), 
                              by = "sender") %>% 
             dplyr::left_join(node_df %>% 
                              dplyr::rename("receiver" = .data$user, "to" = .data$id), 
                              by = "receiver") %>%
             dplyr::rename("weight" = .data$count, "title" = .data$comment) %>% 
             dplyr::select(.data$from, .data$to, .data$weight, .data$comment_id, .data$subreddit, .data$thread_id,
                           .data$title)
  
  # edge_df <- actor_network$edges

  # weight edges network graph
  if (weightEdges) {
    edge_df$comment_id <- edge_df$title <- NULL
    edge_df <- edge_df %>% dplyr::group_by(.data$from, .data$to) %>% 
               dplyr::summarise(weight = sum(.data$weight)) %>% dplyr::ungroup()  
  
    appendToName <- "Weighted"
  # include comment text as edge attribute network graph
  } else if (includeTextData) {
    edge_df$weight <- NULL
    
    # rename the edge attribute containing the thread comment
    edge_df <- edge_df %>% dplyr::rename("vosonTxt_comment" = .data$title)
    
    # problem control characters encountered in reddit text
    # edge_df$vosonTxt_comment <- gsub("[\x01\x05\x18\x19\x1C]", "", edge_df$vosonTxt_comment, perl = TRUE)
    appendToName <- "Txt"
    
    if (cleanText) {
      edge_df$vosonTxt_comment <- gsub("[^[:punct:]^[:alnum:]^\\s]", "", edge_df$vosonTxt_comment, perl = TRUE)
      appendToName <- "CleanTxt"
    }
  } else {
    edge_df$title <- edge_df$weight <- NULL
  }
  
  g <- graph_from_data_frame(d = edge_df, vertices = node_df, directed = TRUE)
  
  # set name to actors user name
  V(g)$name <- V(g)$user
  g <- delete_vertex_attr(g, "user")
  g <- set_graph_attr(g, "type", "reddit")
  
  if (writeToFile) {
    name <- paste0("RedditActorNetwork", appendToName)
    writeOutputFile(g, "graphml", name)
  }
  
  cat("\nDone!\n")
  flush.console()
  
  return(g)
}
