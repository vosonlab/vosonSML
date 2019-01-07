#' Create an actor network from social media data
#'
#' This function creates an actor network from social media data collected using the \code{Collect} method. Edges in 
#' the network represent interactions or relationships between the actors. For example, with twitter data an 
#' interaction is defined as a 'mention', reply' or 'retweet' from user i to user j, given 'tweet' m. With youtube 
#' comments, an interaction is defined as a 'reply' from user i to user j, given 'comment' m. The resulting network is 
#' returned as an igraph object.
#'
#' @param x Collected social media data with \code{social media} class attribute.
#' @param ... Additional parameters to pass to the network creation method.
#' @param writeToFile Logical. Save network data to a file in the current working directory. Default is \code{FALSE}.
#'
#' @seealso \code{\link{Create}}
#' @keywords create actor twitter youtube reddit
#'
#' @export
CreateActorNetwork <- function(x, ...) {
  # searches the class list of x for matching method
  UseMethod("CreateActorNetwork", x)
}

#' @rdname CreateActorNetwork
#' @export
CreateActorNetwork.default <- function(x, ...) {
  cat("Cannot create actor network using this type of data.\n")
  
  if (inherits(x, "temporal")) {
    cat("The data you supplied is temporal. Please use the CreateDynamicNetwork function for temporal data.\n")
  }
}
