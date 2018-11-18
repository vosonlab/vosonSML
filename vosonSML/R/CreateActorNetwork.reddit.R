#' Creates a reddit actor network from collected threads
#'
#' Uses RedditExtractoR::user_network to create an igraph actor network with posts as edge attribute.
#'
#' @param x a dataframe as vosonSML class object containing collected social network data
#' @param includeTextData logical. If the igraph network edges should include the text attribute.
#' @param cleanText logical. If non-alphanumeric, non-punctuation, and non-space characters should be removed from the 
#' included text attribute data.
#' @param writeToFile logical. If the igraph network graph should be written to file.
#' 
#' @return an igraph object of the actor network
#'
CreateActorNetwork.reddit <- function(x, includeTextData, cleanText, writeToFile) {
  
  if (missing(writeToFile)) {
    writeToFile <- FALSE
  }
  
  if (missing(includeTextData) || includeTextData != TRUE) {
    includeTextData <- FALSE
  }
  
  if (includeTextData) {
    if (missing(cleanText) || cleanText != FALSE) {
      cleanText <- TRUE
    }
  }
  
  thread_df <- x
  actor_network <- RedditExtractoR::user_network(thread_df, include_author = TRUE, agg = FALSE)
  
  edge_df <- actor_network$edges
  
  if (includeTextData) {
    # rename the edge attribute containing the thread post
    edge_df <- edge_df %>% dplyr::rename("vosonTxtPost" = .data$title)
    
    if (cleanText) {
      edge_df$vosonTxtPost <- gsub("[^[:punct:]^[:alnum:]^\\s]", "", edge_df$vosonTxtPost, perl = TRUE) 
    }
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
  
  if (writeToFile) {
    name <- "RedditActorNetwork"
    if (includeTextData) { 
      name <- paste0(name, "Txt")
    }
    writeOutputFile(g, "graphml", name)
  }
  
  cat("\nDone!\n")
  flush.console()
  
  return(g)
}
