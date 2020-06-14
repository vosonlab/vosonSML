#' @title Create youtube actor network
#' 
#' @description Creates a youtube actor network from comment threads on youtube videos. Users who have made comments to 
#' a video (top-level comments) and users who have replied to those comments are actor nodes. The comments are 
#' represented as directed edges between the actors. The video id is also included as an actor node, representative of 
#' the videos publisher with top-level comments as directed edges towards them. 
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"youtube"} class names.
#' @param type Character string. Type of network to be created, set to \code{"actor"}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#' 
#' @examples
#' \dontrun{
#' # create a youtube actor network graph
#' actorNetwork <- youtubeData %>% Create("actor")
#' 
#' # network
#' # actorNetwork$nodes
#' # actorNetwork$edges
#' }
#' 
#' @export
Create.actor.youtube <- function(datasource, type, ...) {
  cat("Generating youtube actor network...")
  
  # df <- tibble::as_tibble(datasource)
  # df <- datasource
  # df <- data.table(datasource)
  class(datasource) <- rmCustCls(class(datasource))
  
  # nodes are authors and videos, edges are comments and self-loops
  
  parent_authors <- datasource %>% dplyr::select(.data$CommentID, .data$AuthorChannelID) %>% 
    dplyr::distinct(.data$CommentID, .keep_all = TRUE) %>% 
    dplyr::rename("ParentID" = .data$CommentID, "ParentAuthorID" = .data$AuthorChannelID)
  
  df_relations <- datasource %>% 
    dplyr::left_join(parent_authors, by = c("ParentID")) %>%
    dplyr::select(.data$AuthorChannelID, .data$ParentID, .data$ParentAuthorID, .data$VideoID, .data$CommentID) %>%
    dplyr::mutate(edge_type = case_when((!is.na(.data$ParentID)) ~ "reply-comment", TRUE ~ "comment")) %>%
    dplyr::mutate(to = if_else(.data$edge_type == "reply-comment", .data$ParentAuthorID,
                               if_else(.data$edge_type == "comment", paste0("VIDEOID:", .data$VideoID),
                                       as.character(NA)))) %>%
    
    dplyr::rename("from" = .data$AuthorChannelID, "video_id" = .data$VideoID, "comment_id" = .data$CommentID) %>%
    dplyr::select(.data$from, .data$to, .data$video_id, .data$comment_id, .data$edge_type)
  
  df_nodes <- datasource %>% dplyr::select(.data$AuthorChannelID, .data$AuthorDisplayName) %>%
    dplyr::distinct(.data$AuthorChannelID, .keep_all = TRUE) %>%
    dplyr::mutate(node_type = "actor") %>%
    dplyr::rename("id" = .data$AuthorChannelID, "screen_name" = .data$AuthorDisplayName)
  
  video_ids <- datasource %>% distinct(.data$VideoID) %>% dplyr::mutate(id = paste0("VIDEOID:", .data$VideoID)) %>%
    dplyr::rename(video_id = .data$VideoID)
  
  df_relations <- dplyr::bind_rows(df_relations, 
                                   video_ids %>% dplyr::mutate(from = .data$id, to = .data$id, 
                                                               edge_type = "self-loop", id = NULL))
  
  video_ids <- video_ids %>% dplyr::select(-.data$video_id)
  
  if (nrow(video_ids)) {
    video_ids %<>% dplyr::mutate(node_type = "video")
    df_nodes <- dplyr::bind_rows(df_nodes, dplyr::anti_join(video_ids, df_nodes, by = "id"))
  }
  
  func_output <- list(
    "nodes" = df_nodes,
    "edges" = df_relations
  )
  
  class(func_output) <- append(class(func_output), c("network", "actor", "youtube"))
  cat("Done.\n")
  
  func_output
}
