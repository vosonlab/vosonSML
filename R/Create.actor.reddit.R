#' @title Create reddit actor network
#' 
#' @description Creates a reddit actor network from thread comments on subreddits. Users who have commented on a thread 
#' are actor nodes and comment replies to each other are represented as directed edges.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"reddit"} class names.
#' @param type Character string. Type of network to be created, set to \code{"actor"}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#' 
#' @examples
#' \dontrun{
#' # create a reddit actor network graph with comment text as edge attributes
#' actorNetwork <- redditData %>% Create("actor")
#' 
#' # network
#' # actorNetwork$nodes
#' # actorNetwork$edges
#' }
#' 
#' @export
Create.actor.reddit <- function(datasource, type, ...) {
  cat("Generating reddit actor network...")
  
  # df <- tibble::as_tibble(datasource) 
  # df <- datasource 
  class(datasource) <- rmCustCls(class(datasource))
  
  # df_thread <- datasource

  # actor_network <- RedditExtractoR::user_network(df_thread, include_author = TRUE, agg = FALSE)

  # modified from RedditExtractoR::user_network to include the df comment id, subreddit and thread id as edge 
  # attributes to support post-processing. df_relations, df_nodes, and df_edges based on network by @ivan-rivera.
  # include_author <- TRUE

  # select cols and rename id and user
  df_relations <- datasource %>% 
    dplyr::select(.data$id, .data$subreddit, .data$thread_id, .data$comm_id, .data$structure,
                  .data$user, .data$author) %>% 
    dplyr::rename("comment_id" = .data$id, "sender" = .data$user)

  df_relations %<>%
    # response_to = "" if structure doesnt contain an underscore
    # else set to structure minus last digit '1_1_2' response_to = '1_1' 
    dplyr::mutate(response_to = ifelse(!grepl("_", .data$structure), "", gsub("_\\d+$", "", .data$structure))) %>%
    
    # select structure and user from original df
    # rename structure to response_to and user to receiver
    # left join df_relations to response_to, receiver by response_to
    # FIXED: crossing threads by joining only on structure (response_to)
    dplyr::left_join(datasource %>% 
      dplyr::select(.data$thread_id, .data$structure, .data$user) %>%
      dplyr::rename("response_to" = .data$structure, "receiver" = .data$user), by = c("response_to", "thread_id"))

  df_relations %<>%
    # inserts author into missing receiver values
    # FIXED: coalesce was crossing threads
    # dplyr::mutate(receiver = dplyr::coalesce(.data$receiver, ifelse(include_author, .data$author, ""))) %>%
    dplyr::mutate(receiver = ifelse(is.na(.data$receiver), .data$author, .data$receiver)) %>%
  
    # filter out when sender and receiver same, or if either deleted or empty string
    # dplyr::filter(.data$sender != .data$receiver, 
    #               !(.data$sender %in% c("[deleted]", "")), 
    #               !(.data$receiver %in% c("[deleted]", ""))) %>%
    # have to decide on deleted, self loops are fine
    # removed comments have the value "[removed]"
    # dplyr::mutate(count = 1) %>%
    dplyr::select(.data$sender, .data$receiver, .data$comment_id, .data$subreddit, .data$comm_id, .data$thread_id)

  # attempt to add authors thread posts as self-loops
  authors <- dplyr::select(datasource, .data$subreddit, .data$thread_id, .data$author) %>% dplyr::distinct() %>%
    dplyr::mutate(sender = .data$author, receiver = .data$author, comment_id = 0, author = NULL)
  
  df_relations <- dplyr::bind_rows(df_relations, authors)
  
  df_nodes <- data.frame(user = with(df_relations, { unique(c(sender, receiver)) }), stringsAsFactors = FALSE) %>%
                tibble::as_tibble() %>% 
                dplyr::mutate(id = as.integer(dplyr::row_number())) %>%  # dplyr::row_number() - 1
                dplyr::select(.data$id, .data$user)

  df_relations %<>% 
    dplyr::left_join(df_nodes %>% 
      dplyr::rename("sender" = .data$user, "from" = .data$id), by = c("sender" = "sender")) %>% 
    dplyr::left_join(df_nodes %>% dplyr::rename("receiver" = .data$user, "to" = .data$id), 
      by = c("receiver" = "receiver")) %>%
    dplyr::select(.data$from, .data$to, .data$subreddit, .data$thread_id, .data$comment_id, .data$comm_id)

  func_output <- list(
    "nodes" = df_nodes,
    "edges" = df_relations
  )
  
  class(func_output) <- append(class(func_output), c("network", "actor", "reddit"))
  cat("Done.\n")
  
  func_output
}
