#' @title Create reddit actor network
#' 
#' @description Creates a reddit actor network from thread comments on subreddits. Users who have commented on a thread 
#' are actor nodes and comment replies to each other are represented as directed edges. Optionally a graph with edge 
#' weights (\code{weight}) or edge text attributes (\code{vosonTxt_comment}) can be created.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"reddit"} class names.
#' @param type Character string. Type of network to be created, set to \code{"actor"}.
#' @param weightEdges Logical. Combine and weight directed network edges. Default is \code{FALSE}.
#' @param textData Logical. Include comment text as an edge attributes of returned igraph network. Is set to 
#' \code{FALSE} if the \code{weightEdges} parameter is \code{TRUE} as text merging is not supported. Default is 
#' \code{FALSE}.
#' @param cleanText Logical. Simple removal of non-alphanumeric, non-punctuation, and non-space characters from 
#' the comment text data applied as graph edge attribute. Not suitable in some cases requiring capture of utf or emoji 
#' characters. Implemented to support basic text analysis and to prevent reddit specific XML control character errors 
#' in the graphml files created by this function. Alternatively custom cleaning of text data can be performed on the 
#' \code{datasource} dataframe before being passed to this function. Default is \code{TRUE}.
#' @param writeToFile Logical. Save network data to a file in the current working directory. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Named list containing generated network as igraph object \code{$graph}.
#' 
#' @examples
#' \dontrun{
#' # create a reddit actor network graph with comment text as edge attributes
#' actorNetwork <- redditData %>% 
#'   Create("actor", includeTextData = TRUE, writeToFile = TRUE)
#' 
#' # igraph object
#' # actorNetwork$graph
#' }
#' 
#' @export
Create.actor.reddit <- function(datasource, type, weightEdges = FALSE, textData = FALSE, cleanText = TRUE, 
                                writeToFile = FALSE, ...) {

  # cleanText is default to TRUE as reddit comments often contain forbidden XML control characters

  # if weightEdges then set textData to FALSE
  if (weightEdges) { textData <- FALSE }

  if (textData == FALSE) { cleanText <- FALSE }

  cat(paste0("Generating reddit actor network", ifelse(textData, " with text edge attributes", ""), "...\n"))
  flush.console()

  # append string to file name to indicate different graph types, only used if writeToFile = TRUE
  append_type <- ""
  thread_df <- datasource

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
    # response_to = "" if structure doesnt contain an underscore
    # else set to structure minus last digit '1_1_2' response_to = '1_1' 
    dplyr::mutate(response_to = ifelse(!grepl("_", .data$structure), "", gsub("_\\d+$", "", .data$structure))) %>%
    
    # select structure and user from original df
    # rename structure to response_to and user to receiver
    # left join sender_receiver_df to response_to, receiver by response_to
    # FIXED: crossing threads by joining only on structure (response_to)
    dplyr::left_join(thread_df %>% 
                     dplyr::select(.data$thread_id, .data$structure, .data$user) %>%
                     dplyr::rename("response_to" = .data$structure, "receiver" = .data$user),
                     by = c("response_to" = "response_to", "thread_id" = "thread_id"))

  # above seems correct ^
  
  sender_receiver_df %<>%
    # inserts author into missing receiver values
    # FIXED: coalesce was crossing threads
    # dplyr::mutate(receiver = dplyr::coalesce(.data$receiver, ifelse(include_author, .data$author, ""))) %>%
    dplyr::mutate(receiver = ifelse(is.na(.data$receiver), .data$author, .data$receiver)) %>%
  
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
                              by = c("sender" = "sender")) %>% 
             dplyr::left_join(node_df %>% 
                              dplyr::rename("receiver" = .data$user, "to" = .data$id), 
                              by = c("receiver" = "receiver")) %>%
             dplyr::rename("weight" = .data$count, "title" = .data$comment) %>% 
             dplyr::select(.data$from, .data$to, .data$weight, .data$comment_id, .data$subreddit, .data$thread_id,
                           .data$title)

  # weight edges network graph
  if (weightEdges) {
    # drop comment id and text
    edge_df$comment_id <- edge_df$title <- NULL
    edge_df %<>% dplyr::group_by(.data$from, .data$to) %>% 
                 dplyr::summarise(weight = sum(.data$weight)) %>% dplyr::ungroup()  

    append_type <- "Weighted"
  # include comment text as edge attribute network graph
  } else if (textData) {
    edge_df$weight <- NULL

    # rename the edge attribute containing the thread comment
    edge_df %<>% dplyr::rename("vosonTxt_comment" = .data$title)

    # problem control characters encountered in reddit text
    # edge_df$vosonTxt_comment <- gsub("[\x01\x05\x18\x19\x1C]", "", edge_df$vosonTxt_comment, perl = TRUE)
    append_type <- "Txt"

    # remove any characters that are not in punctuation, alphanumeric classes or spaces
    if (cleanText) {
      edge_df$vosonTxt_comment <- gsub("[^[:punct:]^[:alnum:]^\\s]", "", edge_df$vosonTxt_comment, perl = TRUE)
      append_type <- "CleanTxt"
    }
  } else {
    edge_df$title <- edge_df$weight <- NULL
  }

  g <- graph_from_data_frame(d = edge_df, vertices = node_df, directed = TRUE)

  # set name to actors user name
  V(g)$name <- V(g)$label <- V(g)$user
  g <- delete_vertex_attr(g, "user")
  g <- set_graph_attr(g, "type", "reddit")

  if (writeToFile) {
    name <- paste0("RedditActorNetwork", append_type)
    writeOutputFile(g, "graphml", name)
  }

  cat("Done.\n")
  flush.console()

  func_output <- list(
    "graph" = g
  )
  
  class(func_output) <- append(class(func_output), c("network", "actor", "reddit"))
  
  return(func_output)
}
