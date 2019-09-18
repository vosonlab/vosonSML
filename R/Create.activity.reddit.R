#' @title Create reddit activity network
#'
#' @description Creates a reddit activity network from subreddit thread comments. Nodes are comments and initial thread
#' posts, edges form the discussion structure and signify to which comment or post a comment has been made to.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"reddit"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#' 
#' @examples
#' \dontrun{
#' # create a reddit activity network graph
#' activityNetwork <- redditData %>% Create("activity")
#' 
#' # network
#' # activityNetwork$nodes
#' # activityNetwork$edges
#' }
#' 
#' @export
Create.activity.reddit <- function(datasource, type, verbose = TRUE, ...) {
  df <- tibble::as_tibble(datasource) 
  
  df_stats <- networkStats(NULL, "collected reddit comments", nrow(df))
  
  cat("Generating reddit activity network...\n")
  flush.console()
  
  # would be better with the unique comment fullname ids
  # comment id format <thread_id>.<structure>
  df %<>% dplyr::mutate(comment_id = paste0(.data$thread_id, ".", .data$structure))
  
  # edges  
  df_relations <- df %>%
    dplyr::rename("from" = .data$comment_id) %>%
    dplyr::mutate(to = ifelse(!grepl("_", .data$structure), paste0(.data$thread_id, ".0"), 
                              gsub("_\\d+$", "", .data$from))) %>%
    dplyr::mutate(edge_type = "comment") %>%
    dplyr::select(.data$from, .data$to, .data$edge_type)
  
  # nodes
  df_nodes <- df %>% dplyr::select(.data$comment_id, .data$comm_date, .data$subreddit, .data$user) %>%
    dplyr::rename("id" = .data$comment_id, "date" = .data$comm_date) %>%
    dplyr::mutate(node_type = "comment")
  
  # add thread posts to nodes
  thread_ids <- df %>% dplyr::select(.data$subreddit, .data$author, .data$thread_id, .data$post_date) %>%
    dplyr::mutate(id = paste0(.data$thread_id, ".0")) %>% 
    dplyr::distinct(.data$subreddit, .data$id, .keep_all = TRUE) %>%
    dplyr::rename("user" = .data$author, "date" = .data$post_date) %>%
    dplyr::select(.data$id, .data$date, .data$subreddit, .data$user) %>%
    dplyr::mutate(node_type = "thread")
  
  df_nodes <- dplyr::bind_rows(df_nodes, dplyr::anti_join(thread_ids, df_nodes, by = c("id", "subreddit")))
  
  df_stats <- networkStats(df_stats, "threads", nrow(thread_ids))
  df_stats <- networkStats(df_stats, "comments", nrow(df))
  df_stats <- networkStats(df_stats, "network nodes", nrow(df_nodes))
  df_stats <- networkStats(df_stats, "network edges", nrow(df_relations))  
  
  # print stats
  if (verbose) { networkStats(df_stats, print = TRUE) }
  
  cat("Done.\n")
  flush.console()
  
  func_output <- list(
    "nodes" = df_nodes,
    "edges" = df_relations
  )
  
  class(func_output) <- append(class(func_output), c("network", "activity", "reddit"))
  
  return(func_output)  
}
