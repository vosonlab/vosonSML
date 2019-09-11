#' @export
Graph <- function(net, directed = TRUE, writeToFile = FALSE, ...) {
  # create igraph object from dataframes
  g <- igraph::graph_from_data_frame(d = net$edges, directed = directed, vertices = net$nodes)
  
  # searches the class list of net for matching method
  UseMethod("Graph", net)
}

#' @export
Graph.default <- function(...) {
  stop("Unknown network type passed to graph.", call. = FALSE) 
}

#' @noRd
#' @method Graph activity
#' @export
Graph.activity <- function(net, directed = TRUE, writeToFile = FALSE, ...) {
  UseMethod("Graph.activity", net)
}

#' @noRd
#' @export
Graph.activity.default <- function(...) {
  stop("Unknown social media type passed to graph.", call. = FALSE)
}

#' @noRd
#' @export
Graph.activity.twitter <- function(net, directed = TRUE, writeToFile = FALSE, ...) {

  igraph::V(g)$label <- ifelse(!is.na(igraph::V(g)$screen_name), 
                               paste0(igraph::V(g)$name, " (", igraph::V(g)$screen_name, ")"), 
                               igraph::V(g)$name)
  
  if (is.logical(writeToFile) && writeToFile) { 
    writeOutputFile(g, "graphml", "TwitterActivity") 
  } else if (is.character(writeToFile)) {
    writeOutputFile(g, "graphml", writeToFile)
  } 
  
  g
}

#' @noRd
#' @export
Graph.activity.youtube <- function(net, directed = TRUE, writeToFile = FALSE, ...) {
  
  igraph::V(g)$label <- ifelse(!is.na(igraph::V(g)$User), 
                               paste0(igraph::V(g)$name, " (", igraph::V(g)$User, ")"), 
                               igraph::V(g)$name)
  
  if (is.logical(writeToFile) && writeToFile) { 
    writeOutputFile(g, "graphml", "YoutubeActivity") 
  } else if (is.character(writeToFile)) {
    writeOutputFile(g, "graphml", writeToFile)
  }
  
  g
}

#' @noRd
#' @export
Graph.activity.reddit <- function(net, directed = TRUE, writeToFile = FALSE, ...) {
  
  V(g)$label <- ifelse(!is.na(V(g)$user), paste0(V(g)$name, " (", V(g)$user, ")"), V(g)$name)

  if (is.logical(writeToFile) && writeToFile) { 
    writeOutputFile(g, "graphml", "RedditActivity") 
  } else if (is.character(writeToFile)) {
    writeOutputFile(g, "graphml", writeToFile)
  }
  
  g
}

# @param weightEdges Logical. Combine and weight directed network edges. Default is \code{FALSE}.
# @param textData Logical. Include comment text as an edge attributes of returned igraph network. Is set to 
# \code{FALSE} if the \code{weightEdges} parameter is \code{TRUE} as text merging is not supported. Default is 
# \code{FALSE}.
# @param cleanText Logical. Simple removal of non-alphanumeric, non-punctuation, and non-space characters from 
# the comment text data applied as graph edge attribute. Not suitable in some cases requiring capture of utf or emoji 
# characters. Implemented to support basic text analysis and to prevent reddit specific XML control character errors 
# in the graphml files created by this function. Alternatively custom cleaning of text data can be performed on the 
# \code{datasource} dataframe before being passed to this function. Default is \code{TRUE}.
# @param writeToFile Logical. Save network data to a file in the current working directory. Default is \code{FALSE}.
