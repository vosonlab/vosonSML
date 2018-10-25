#' Creates a reddit actor network from threads.
#'
#' Uses RedditExtractoR::user_network to create an igraph actor network with posts as edge attribute.
#' 
#' @param x A dataframe as vosonSML class object containing collected social network data.
#' @param write_to_file Boolean. If the igraph network graph should be written to file. 
#' @return An igraph object of the actor network.
#' 
CreateActorNetwork.reddit <- function(x, writeToFile) {
    
  if (missing(writeToFile)) {
    writeToFile <- FALSE
  }
  
  thread_df <- x
  actor_network <- RedditExtractoR::user_network(thread_df, include_author = TRUE, agg = FALSE)
  
  node_df <- actor_network$nodes
  edge_df <- actor_network$edges
  
  # rename the edge attribute containing the thread post
  edge_df <- edge_df %>% rename("vosonTxtPost" = .data$title)
  
  g <- igraph::graph_from_data_frame(d = edge_df, vertices = node_df, directed = TRUE)
  
  # set name to actors user name
  V(g)$name <- V(g)$user
  
  if (writeToFile == TRUE) {
    current_time <- format(Sys.time(), "%b_%d_%H_%M_%S_%Y")
    save_file_name <- paste0(current_time, "_RedditActorNetwork.graphml")
    write.graph(g, save_file_name, format = "graphml")
    
    cat(paste0("Reddit network graph was written to working directory, with filename:\n", save_file_name, "\n"))
  }
  
  cat("\nDone!\n")
  flush.console()
  
  return(g)  
}