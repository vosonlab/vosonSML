#' Create actor networks from social media data
#'
#' This function creates a unimodal 'actor' network from social media data (i.e. from data frames of class dataSource, 
#' or for Twitter data it is also possible to provide a list of data frames). In this actor network, edges represent 
#' relationships between actors of the same type (e.g. interactions between Twitter users). For example, with Twitter 
#' data an interaction is defined as a 'mention' or 'reply' or 'retweet' from user i to user j, given 'tweet' m. With 
#' YouTube comments, an interaction is defined as a 'reply' or 'mention' from user i to user j, given 'comment' m.
#'
#' This function creates a (weighted and directed) unimodal 'actor' network from a data frame of class dataSource 
#' (which are created using the CollectData family of functions in the vosonSML package), or a list of Twitter data 
#' frames collected using CollectDataTwitter function.
#'
#' The resulting network is an igraph graph object. This graph object is unimodal because edges represent relationships 
#' between vertices of the same type (read: actors), such as replies/retweets/mentions between Twitter users. Edges are 
#' directed and weighted (e.g. if user i has replied n times to user j, then the weight of this directed edge equals n).
#'
#' @param x a data frame of class dataSource. For Twitter data, it is also possible to provide a list of data frames 
#' (i.e. data frames that inherit class dataSource and twitter). Only lists of Twitter data frames are supported at 
#' this time. If a list of data frames is provided, then the function binds these row-wise and computes over the entire 
#' data set.
#' @param writeToFile logical. If TRUE then the network is saved to file in current working directory (GRAPHML format), 
#' with filename denoting the current date/time and the type of network
#' @param ... additional parameters to pass to the network creation method
#' @return an igraph graph object, with directed and weighted edges
#' 
#' @note Not all data sources in vosonSML can be used for creating actor networks.
#' Currently supported data sources are: YouTube, Twitter
#'
#' Other data sources (e.g. Facebook) will be implemented in the future. The user is notified if they try to create 
#' actor networks for incompatible data sources.
#'
#' For Twitter data, actor networks can be created from multiple data frames (i.e. datasets collected individually 
#' using CollectDataTwitter). Simply create a list of the data frames that you wish to create a network from. For
#' example: my_list <- list(my_twitter_data_1, my_twitter_data_2, my_twitter_data_3)
#'
#' @author Timothy Graham <timothy.graham@@anu.edu.au>, Robert Ackland <robert.ackland@@anu.edu.au>
#'
#' @noRd
CreateActorNetwork <- function(x, writeToFile, ...) {

  if (missing(writeToFile)) {
    writeToFile <- FALSE
  }

  UseMethod("CreateActorNetwork", x)
}
