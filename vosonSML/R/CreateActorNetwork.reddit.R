#' Creates a reddit actor network from threads
#'
#' Uses RedditExtractoR::user_network to create an igraph actor network with posts as edge attribute.
#'
#' @param x a dataframe as vosonSML class object containing collected social network data
#' @param writeToFile logical. If the igraph network graph should be written to file.
#' @param text_data logical. If the igraph network edges should include the text attribute.
#' @return an igraph object of the actor network
#'
#' @noRd
CreateActorNetwork.reddit <- function(x, writeToFile, text_data) {

  if (missing(writeToFile)) {
    writeToFile <- FALSE
  }

  if (missing(text_data) || text_data != TRUE) {
    text_data <- FALSE
  }
  
  thread_df <- x
  actor_network <- RedditExtractoR::user_network(thread_df, include_author = TRUE, agg = FALSE)

  edge_df <- actor_network$edges

  if (text_data) {
    # rename the edge attribute containing the thread post
    edge_df <- edge_df %>% dplyr::rename("vosonTxtPost" = .data$title)    
  } else {
    edge_df$title <- NULL
    edge_df <- edge_df %>% dplyr::group_by(.data$from, .data$to) %>% 
      dplyr::summarise(weight = sum(.data$weight)) %>% dplyr::ungroup()
  }

  g <- graph_from_data_frame(d = edge_df, vertices = actor_network$nodes, directed = TRUE)

  # set name to actors user name
  V(g)$name <- V(g)$user
  g <- delete_vertex_attr(g, "user")
  g <- set_graph_attr(g, "type", "reddit")

  if (isTrueValue(writeToFile)) {
    writeOutputFile(g, "graphml", "RedditActorNetwork")
  }
  
  cat("\nDone!\n")
  flush.console()

  return(g)
}
