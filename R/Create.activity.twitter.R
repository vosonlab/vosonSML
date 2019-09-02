#' @title Create twitter activity network
#' 
#' @description Creates a twitter activity network from tweets.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param writeToFile Logical. Save network data to a file in the current working directory. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return Graph
#' 
#' @examples
#' \dontrun{
#' # create a twitter activity network graph
#' }
#' 
#' @export
Create.activity.twitter <- function(datasource, type, writeToFile = FALSE, verbose = FALSE, ...) {
  
  # df <- datasource
  df <- tibble::as_tibble(datasource) # data.table(df)
  
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
    dplyr::select(.data$from, .data$to, .data$edge_type) %>%
    dplyr::filter(.data$edge_type != "tweet") # remove stand alone tweets as they have no relations
  
  # vertices
  df_nodes <- unique(c(df$reply_to_status_id,
                df$quoted_status_id,
                df$retweet_status_id))
  df_nodes <- tibble::enframe(na.omit(df_nodes), name = NULL, value = "status_id")
  # df_nodes only has one column at this point suppress join warning
  df_nodes <- suppressWarnings(dplyr::anti_join(df_nodes, df, by = "status_id"))
  df_nodes <- dplyr::bind_rows(df, df_nodes)
  df_nodes <- df_nodes[, c(2, 1, 3:ncol(df_nodes))] %>%
    dplyr::rename("vosonTxt_tweet" = .data$text, 
                  "user_name" = .data$name)
  # handle igraph warnings due to dttm class columns
  df_nodes <- dplyr::mutate_at(df_nodes, vars(contains('created_at')), as.character)

  df_replies <- df %>% dplyr::filter(!is.na(.data$reply_to_status_id)) %>% dplyr::select(starts_with("reply_to_")) %>%
    dplyr::rename("status_id" = .data$reply_to_status_id,
                  "user_id" = .data$reply_to_user_id,
                  "screen_name" = .data$reply_to_screen_name)
  
  test_nodes <- df_nodes
  
  test_nodes <- dplyr::semi_join(test_nodes, df_replies)
  
  df_quotes <- df %>% dplyr::filter(!is.na(.data$quoted_status_id)) %>% dplyr::select(starts_with("quoted"))
  df_retweets <- df %>% dplyr::filter(!is.na(.data$retweet_status_id)) %>% dplyr::select(starts_with("retweet"))
  
  g <- igraph::graph_from_data_frame(d = df_relations, directed = TRUE, vertices = df_nodes)
  
  V(g)$label <- ifelse(!is.na(V(g)$screen_name), paste0(V(g)$name, " (", V(g)$screen_name, ")"), V(g)$name)
  
  g <- igraph::set_graph_attr(g, "type", "twitter")
  
  if (writeToFile) { writeOutputFile(g, "graphml", "TwitterActivityNetwork") }
  
  cat("Done.\n")
  flush.console()
  
  func_output <- list(
    "relations" = df_relations,
    "users" = df_nodes,
    "graph" = g
  )
  
  class(func_output) <- append(class(func_output), c("network", "activity", "twitter"))
  
  return(func_output)
}
