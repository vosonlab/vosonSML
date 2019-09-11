#' @title Create youtube activity network
#' 
#' @description Creates an activity network from collected youtube video comment threads. Nodes are top-level
#' comments, reply comments and videos. Edges are directed between the nodes and represent commenting activity.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"youtube"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param writeToFile Logical. Save network data to a file in the current working directory. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return Named list containing generated network as igraph object \code{$graph}.
#' 
#' @examples
#' \dontrun{
#' # create a youtube activity network graph
#' activityNetwork <- youtubeData %>% Create("activity", writeToFile = TRUE)
#' 
#' # igraph object
#' # activityNetwork$graph
#' }
#' 
#' @export
Create.activity.youtube <- function(datasource, type, writeToFile = FALSE, verbose = TRUE, ...) {
  df <- tibble::as_tibble(datasource) 
  
  df_stats <- networkStats(NULL, "collected youtube comments", nrow(df))
  
  cat("Generating youtube activity network...\n")
  flush.console()
  
  # edges
  df_relations <- df %>% dplyr::select(.data$CommentId, .data$ParentID, .data$VideoID) %>%
    dplyr::mutate(edge_type = case_when((.data$ParentID != "None") ~ "reply-comment", TRUE ~ "comment")) %>%
    dplyr::mutate(to = if_else(.data$edge_type == "reply-comment", .data$ParentID, 
                               if_else(.data$edge_type == "comment", .data$VideoID,
                                       as.character(NA)))) %>%
    # removes parent id part of <parent id>.<comment id> id format for reply comments
    # dplyr::rowwise() %>%
    # dplyr::mutate(from = if_else(.data$edge_type == "reply-comment", 
    #                              sub(paste0(.data$ParentID, "\\."), "", .data$CommentId), 
    #                              if_else(.data$edge_type == "comment", .data$CommentId,
    #                                      as.character(NA)))) %>%
    dplyr::rename(from = .data$CommentId) %>%
    dplyr::select(.data$from, .data$to, .data$edge_type)
  
  # vertices
  df_nodes <- df %>% dplyr::select(.data$CommentId, .data$ParentID, .data$PublishTime, .data$VideoID, .data$User) %>%
    dplyr::mutate(node_type = case_when((.data$ParentID != "None") ~ "reply-comment", TRUE ~ "comment"))
    # dplyr::rowwise() %>%
    # dplyr::mutate(id = if_else(.data$ParentID != "None", 
    #                                      sub(paste0(.data$ParentID, "\\."), "", .data$CommentId),
    #                                      .data$CommentId)) %>%
  
  # add unique parent ids not already in node list
  parent_ids <- dplyr::distinct(df_nodes, .data$ParentID) %>% dplyr::filter(.data$ParentID != "None") %>%
    dplyr::rename(CommentId = .data$ParentID) %>% 
    dplyr::mutate(node_type = "comment") # node type for parent ids are comment
  
  if (nrow(parent_ids)) {
    df_nodes <- dplyr::bind_rows(df_nodes, dplyr::anti_join(parent_ids, df_nodes, by = "CommentId"))
  }
  
  # add unique video ids not already in node list
  video_ids <- dplyr::distinct(df_nodes, .data$VideoID) %>% dplyr::rename(CommentId = .data$VideoID) %>%
    dplyr::mutate(node_type = "video")
  
  if (nrow(video_ids)) {
    df_nodes <- dplyr::bind_rows(df_nodes, dplyr::anti_join(video_ids, df_nodes, by = "CommentId"))
  }  
  
  df_nodes <- dplyr::select(df_nodes, .data$CommentId, .data$PublishTime, .data$User, .data$node_type) %>%
    dplyr::mutate_at(vars(contains('PublishTime')), as.character)
  
  node_summary <- df_nodes %>% dplyr::group_by(.data$node_type) %>%
    summarise(num = dplyr::n())
  
  for (row in 1:nrow(node_summary)) {
    type <- node_summary[row, "node_type"]
    if (type == "comment") df_stats <- networkStats(df_stats, "top-level comments", node_summary[row, "num"])
    else if (type == "reply-comment") df_stats <- networkStats(df_stats, "reply comments", node_summary[row, "num"])
    else if (type == "video") df_stats <- networkStats(df_stats, "videos", node_summary[row, "num"])
  }
  df_stats <- networkStats(df_stats, "network nodes", nrow(df_nodes))
  df_stats <- networkStats(df_stats, "network edges", nrow(df_relations))  
  
  # print stats
  if (verbose) { networkStats(df_stats, print = TRUE) }
  
  # g <- igraph::graph_from_data_frame(d = df_relations, directed = TRUE, vertices = df_nodes)
  # 
  # V(g)$label <- ifelse(!is.na(V(g)$User), paste0(V(g)$name, " (", V(g)$User, ")"), V(g)$name)
  # 
  # g <- igraph::set_graph_attr(g, "type", "youtube")
  # 
  # if (writeToFile) { writeOutputFile(g, "graphml", "YoutubeActivityNetwork") }
  
  cat("Done.\n")
  flush.console()
  
  func_output <- list(
    "edges" = df_relations,
    "nodes" = df_nodes # ,
    # "graph" = g
  )
  
  class(func_output) <- append(class(func_output), c("network", "activity", "youtube"))
  
  return(func_output)  
}
