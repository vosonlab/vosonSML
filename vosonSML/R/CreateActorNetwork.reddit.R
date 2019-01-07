# Creates a reddit actor network
# 
# Uses RedditExtractoR::user_network to create an igraph directed actor network with comment ids as
# edge attributes.
#
#' @param weightEdges Logical. Combines and weights directed network edges. Default is \code{FALSE}.
#' @param textData Logical. If the igraph network should include the comment text as an edge attribute. 
#' Cannot be used with the \code{weightEdges} parameter. Default is \code{FALSE}.
#' @param cleanText Logical. If non-alphanumeric, non-punctuation, and non-space characters should be removed from the 
#' included text attribute data. Only applies if \code{textData = TRUE}. Default is \code{TRUE}.
#' 
#' @return A reddit actor network as igraph object.
#' 
#' @rdname CreateActorNetwork
#' @export
CreateActorNetwork.reddit <- function(x, weightEdges = FALSE, textData = FALSE, cleanText = TRUE, 
                                      writeToFile = FALSE, ...) {
  
  # default cleanText = TRUE as reddit comments often contain forbidden XML control characters
  
  # if weightEdges then textData set FALSE
  if (weightEdges) { textData <- FALSE }
  
  if (textData == FALSE) { cleanText <- FALSE }
  
  cat("Generating reddit actor network...\n")
  flush.console()
  
  # append string to file name to indicate different graph types, only used if writeToFile = TRUE
  appendToName <- ""
  
  thread_df <- x
  
  # actor_network <- RedditExtractoR::user_network(thread_df, include_author = TRUE, agg = FALSE)
  
  # modified from RedditExtractoR::user_network to include the df comment id, subreddit and thread id as edge 
  # attributes to support post-processing. author of sender_receiver_df, node_df, and edge_df @ivan-rivera.
  include_author <- TRUE
  
  # select cols and rename id and user
  sender_receiver_df <- thread_df %>% 
    dplyr::select(.data$id, .data$subreddit, .data$thread_id, .data$structure, .data$user, .data$author, 
                  .data$comment) %>% 
    dplyr::rename("comment_id" = .data$id, "sender" = .data$user)
  
  sender_receiver_df %<>%
    # response_to = "" if structure doesnt have underscore in it
    # else structure minus last digit '1_1_2' response_to = '1_1' 
    dplyr::mutate(response_to = ifelse(!grepl("_", .data$structure), "", gsub("_\\d+$", "", .data$structure))) %>%
    
    # select structure and user from original df
    # rename structure to response_to and user to receiver
    # left join sender_receiver_df to response_to, receiver by response_to
    dplyr::left_join(thread_df %>% 
                     dplyr::select(.data$structure, .data$user) %>%
                     dplyr::rename("response_to" = .data$structure, "receiver" = .data$user),
                     by = "response_to")
  
  sender_receiver_df %<>%
    # inserts author into missing receiver values
    dplyr::mutate(receiver = dplyr::coalesce(.data$receiver, ifelse(include_author, .data$author, ""))) %>%
    # filter out when sender and receiver same, or if either deleted or empty string
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
    # drop comment id and text
    edge_df$comment_id <- edge_df$title <- NULL
    edge_df %<>% dplyr::group_by(.data$from, .data$to) %>% 
                 dplyr::summarise(weight = sum(.data$weight)) %>% dplyr::ungroup()  
  
    appendToName <- "Weighted"
  # include comment text as edge attribute network graph
  } else if (textData) {
    edge_df$weight <- NULL
    
    # rename the edge attribute containing the thread comment
    edge_df %<>% dplyr::rename("vosonTxt_comment" = .data$title)
    
    # problem control characters encountered in reddit text
    # edge_df$vosonTxt_comment <- gsub("[\x01\x05\x18\x19\x1C]", "", edge_df$vosonTxt_comment, perl = TRUE)
    appendToName <- "Txt"
    
    # remove any characters that are not in punctuation, alphanumeric classes or spaces
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
  
  cat("Done.\n")
  flush.console()
  
  return(g)
}
