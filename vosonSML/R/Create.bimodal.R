#' Create bimodal networks from social media data
#'
#' This function creates a directed and weighted bimodal network from a data frame of class \code{dataSource} (as 
#' created by the CollectData functions).
#'
#' @param datasource A dataframe of class \code{datasource} and class of a social media type.
#' @param type Character string. Type of network to be created.
#' @param ... Additional parameters to pass to the network creation method.
#' 
#' @param removeTermsOrHashtags Character string. Default is none. Otherwise this argument specifies which terms or 
#' hashtags (i.e. vertices with matching 'name') should be removed from the bimodal network. This is useful to remove
#' the search term or hashtag that was used to collect the data (i.e. remove the corresponding vertex in the graph). 
#' For example, a value of "#auspol" means that if there is a vertex with the exact name "#auspol" then this vertex 
#' will be removed.
#' @param writeToFile Logical. If \code{TRUE} then the network is saved to file in current working directory (graphml 
#' format), with filename denoting the current datetime and the type of network.
#' 
#' @note Supported data sources: \code{twitter}
#' 
#' @seealso \code{\link{Create}}
#' @keywords create bimodal network twitter
#'
#' @rdname Create.bimodal
#' @method Create bimodal
#' @export
Create.bimodal <- function(datasource, type, ...) {
  UseMethod("Create.bimodal", datasource)
}

#' @export
Create.bimodal.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create bimodal network.", call. = FALSE) 
}
