#' @title Create twitter activity network
#' 
#' @description Creates a twitter activity network from collected tweets. Nodes are tweets and directed edges represent
#' the relationship of tweets to one another. For example, there is a directed edge from a quote tweet towards the
#' tweet that was quoted. Stand-alone tweets that are not replies, retweets or quote tweets have no relation to others
#' and will be isolates.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#' 
#' @examples
#' \dontrun{
#' # create a twitter activity network graph
#' activityNetwork <- twitterData %>% Create("activity")
#'   
#' # network
#' # activityNetwork$nodes
#' # activityNetwork$edges
#' }
#' 
#' @export
Create.activity.twitter <- function(datasource, type, verbose = TRUE, ...) {
  
  df <- tibble::as_tibble(datasource)
  
  df_stats <- networkStats(NULL, "collected tweets", nrow(df))
  
  cat("Generating twitter activity network...\n")
  flush.console()
  
  # edges
  df_relations <- df %>% dplyr::select(.data$status_id, 
                                 .data$reply_to_status_id,
                                 .data$quoted_status_id,
                                 .data$retweet_status_id) %>%
    dplyr::mutate(edge_type = case_when(!is.na(.data$reply_to_status_id) ~ "reply",
                                   !is.na(.data$quoted_status_id) ~ "quote",
                                   !is.na(.data$retweet_status_id) ~ "retweet",
                                   TRUE ~ "tweet")) %>%
    dplyr::mutate(to = if_else(.data$edge_type == "reply", .data$reply_to_status_id, 
                         if_else(.data$edge_type == "quote", .data$quoted_status_id,
                           if_else(.data$edge_type == "retweet", .data$retweet_status_id, 
                                   as.character(NA))))) %>%
    dplyr::rename("from" = .data$status_id) %>%
    dplyr::select(.data$from, .data$to, .data$edge_type)
  
  edge_summary <- df_relations %>% dplyr::group_by(.data$edge_type) %>%
    summarise(num = dplyr::n())
  
  for (row in 1:nrow(edge_summary)) {
    type <- edge_summary[row, "edge_type"]
    if (type == "tweet") df_stats <- networkStats(df_stats, "tweets", edge_summary[row, "num"])
    else if (type == "retweet") df_stats <- networkStats(df_stats, "retweets", edge_summary[row, "num"])
    else if (type == "quote") df_stats <- networkStats(df_stats, "quote tweets", edge_summary[row, "num"])
    else if (type == "reply") df_stats <- networkStats(df_stats, "reply tweets", edge_summary[row, "num"])
  }
  
  # remove stand alone tweets as they have no relations
  df_relations <- dplyr::filter(df_relations, .data$edge_type != "tweet")
  
  # vertices
  df_nodes <- df %>% dplyr::select(.data$status_id, .data$user_id, .data$screen_name, .data$created_at)
  
  # order of binding rows for nodes in data based on completeness 
  
  df_quotes <- dplyr::select(df, .data$quoted_status_id,
                             .data$quoted_user_id,
                             .data$quoted_screen_name,
                             .data$quoted_created_at) %>%
    dplyr::filter(!is.na(.data$quoted_status_id))  %>%
    dplyr::rename("status_id" = .data$quoted_status_id,
                  "user_id" = .data$quoted_user_id,
                  "screen_name" = .data$quoted_screen_name,
                  "created_at" = .data$quoted_created_at) %>%
    dplyr::distinct(.data$status_id, .keep_all = TRUE)
  
  if (nrow(df_quotes)) {
    df_nodes <- dplyr::bind_rows(df_nodes, dplyr::anti_join(df_quotes, df_nodes, by = "status_id"))
  }
  
  df_replies <- dplyr::select(df, .data$reply_to_status_id,
                              .data$reply_to_user_id,
                              .data$reply_to_screen_name) %>%
    dplyr::filter(!is.na(.data$reply_to_status_id))  %>%
    dplyr::rename("status_id" = .data$reply_to_status_id,
                  "user_id" = .data$reply_to_user_id,
                  "screen_name" = .data$reply_to_screen_name) %>%
    dplyr::distinct(.data$status_id, .keep_all = TRUE)
  
  if (nrow(df_replies)) {
    df_nodes <- dplyr::bind_rows(df_nodes, dplyr::anti_join(df_replies, df_nodes, by = "status_id"))
  }
  
  df_retweets <- dplyr::select(df, .data$retweet_status_id,
                               .data$retweet_created_at) %>%
    dplyr::filter(!is.na(.data$retweet_status_id))  %>%
    dplyr::rename("status_id" = .data$retweet_status_id,
                  "created_at" = .data$retweet_created_at) %>%
    dplyr::distinct(.data$status_id, .keep_all = TRUE)
  
  if (nrow(df_retweets)) {
    df_nodes <- dplyr::bind_rows(df_nodes, dplyr::anti_join(df_retweets, df_nodes, by = "status_id"))
  }
  
  # handle igraph warnings due to dttm class columns
  df_nodes <- dplyr::mutate_at(df_nodes, vars(contains('created_at')), as.character)
  
  df_stats <- networkStats(df_stats, "nodes from data", nrow(df_nodes) - nrow(df))
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
  
  class(func_output) <- union(class(func_output), c("network", "activity", "twitter"))
  
  func_output
}
