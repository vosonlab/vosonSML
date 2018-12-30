#' Create an actor network from social media data
#'
#' This function creates a unimodal 'actor' network from social media data (i.e. from data frames of class
#' \code{dataSource}, or for Twitter data it is also possible to provide a *list* of data frames). In this actor
#' network, edges represent relationships between actors of the same type (e.g. interactions between Twitter users).
#' For example, with Twitter data an interaction is defined as a 'mention' or 'reply' or 'retweet' from user i to
#' user j, given 'tweet' m. With YouTube comments, an interaction is defined as a 'reply' or 'mention' from user i to
#' user j, given 'comment' m.
#'
#' This function creates a (weighted and directed) unimodal 'actor' network from a data frame of class
#' \code{dataSource} (which are created using the 'CollectData' family of functions in the vosonSML package), or a
#' *list* of Twitter data frames collected using the \code{CollectDataTwitter} function.
#'
#' The resulting network is an igraph graph object. This graph object is unimodal because edges represent relationships
#' between vertices of the same type (read: 'actors'), such as replies/retweets/mentions between Twitter
#' users. Edges are directed and weighted (e.g. if user i has replied n times to user j, then the weight of this
#' directed edge equals n).
#'
#' @param x A data frame of class \code{dataSource}. For Twitter data, it is also possible to provide a *list* of data
#' frames (i.e. data frames that inherit class \code{dataSource} and \code{twitter}). Only lists of Twitter data frames
#' are supported at this time. If a list of data frames is provided, then the function binds these row-wise and
#' computes over the entire data set.
#' @param writeToFile Logical. If \code{TRUE} then the network is saved to file in current working directory (GRAPHML
#' format), with filename denoting the current date/time and the type of network.
#' @param ... Additional parameters to pass to the network creation method.
#' @return An igraph graph object, with directed and weighted edges.
#'
#' @note Supported data sources: \code{twitter}, \code{youtube}, \code{reddit}
#'
#' For Twitter data, actor networks can be created from multiple data frames (i.e. datasets collected individually
#' using 'CollectDataTwitter'). Simply create a list of the data frames that you wish to create a network from. For
#' example, \code{myList <- list(myTwitterData1, myTwitterData2, myTwitterData3)}
#'
#' @seealso \code{Create}
#' @keywords create actor twitter youtube reddit
#'
CreateActorNetwork <- function(x, writeToFile, ...) {
  if (missing(writeToFile)) {
    writeToFile <- FALSE
  }
  
  UseMethod("CreateActorNetwork", x)
}
