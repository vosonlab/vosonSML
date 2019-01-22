#' Create networks from social media data
#'
#' This function creates networks from social media data (i.e. collected from dataframes of class \code{social media}).
#' \code{Create} is the final step of the \code{Authenticate}, \code{Collect}, \code{Create} workflow. This function 
#' is a wrapper for the Create Network S3 methods.
#'
#' @param datasource Collected social media data of class \code{datasource} and \code{socialmedia}.
#' @param type Character string. Type of network to be created, can be \code{actor}, \code{bimodal} or \code{semantic}.
#' @param ... Additional parameters for network creation for appropriate \code{socialmedia} and network \code{type}. 
#' Refer to create network S3 methods \code{socialmedia} type for default parameters.
#'
#' @return Named list containing generated network as igraph object.
#'
#' @note When creating twitter networks, a network with additional user information can be generated using the
#' \code{\link{EnhanceNetwork.twitter}} function. Additional calls can be made to the twitter API to get information
#' about users that were identified as nodes during network creation.
#' 
#' @seealso \code{\link{CreateActorNetwork}}, \code{\link{CreateBimodalNetwork}}, \code{\link{CreateSemanticNetwork}}
#' @keywords create actor bimodal semantic network
#'
#' @export
Create <- function(datasource, type = "actor", ...) {
  creator <- switch(tolower(type),
                    actor = CreateActorNetwork,
                    bimodal = CreateBimodalNetwork,
                    semantic = CreateSemanticNetwork,
                    stop("Unknown network type passed to create.", call. = FALSE))
  
  # calls method mapped to type with parameters passed to create
  network_result <- creator(datasource, ...)
  
  # creates class as vector that adds network results class and vosonsml class attributes
  class(network_result) <- append(class(network_result), c("vosonsml"))
  
  return(network_result)
}
