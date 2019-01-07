#' Create networks from social media data
#'
#' This function creates networks from social media data (i.e. collected from dataframes of class \code{social media}).
#' \code{Create} is the final step of the \code{Authenticate}, \code{Collect}, \code{Create} workflow. This function 
#' is a wrapper for the Create*Network S3 methods.
#'
#' @param dataSource Social media data collected using the \code{Collect} method.
#' @param type Character string. Type of network to be created, can be \code{actor}, \code{bimodal},
#' \code{dynamic}, \code{semantic} or \code{ego}.
#' @param ... Additional parameters for network creation for appropriate \code{social media} and network \code{type}. 
#' Refer to S3 methods \code{social media} type for default parameters.
#'
#' @return Network data containing an igraph object.
#'
#' @note When creating twitter networks, a network with additional user information can be generated using the
#' \code{\link{GraphUserInfoTwitter}} function. Additional calls can be made to the twitter API to get information
#' about users that were identified as nodes during network creation.
#' 
#' @seealso \code{\link{CreateActorNetwork}}, \code{\link{CreateBimodalNetwork}}, \code{\link{CreateSemanticNetwork}}
#' @keywords create actor bimodal semantic network
#'
#' @export
Create <- function(dataSource, type = "actor", ...) {
  # if ego is in the class list
  if (inherits(dataSource, "ego")) {
    return(CreateEgoNetworkFromData(dataSource)) # you cannot create actor out of ego data
  }
  
  creator <- switch(tolower(type),
                    actor = CreateActorNetwork,
                    bimodal = CreateBimodalNetwork,
                    dynamic = CreateDynamicNetwork,
                    semantic = CreateSemanticNetwork,
                    ego = CreateEgoNetworkFromData,
                    stop("Unknown Type"))
  
  # calls method mapped to type with parameters passed to create
  networkToReturn <- creator(dataSource, ...)
  
  # creates class as vector that adds network results class and vosonSML class attributes
  class(networkToReturn) <- append(class(networkToReturn), c("vosonSML"))
  
  return(networkToReturn)
}
