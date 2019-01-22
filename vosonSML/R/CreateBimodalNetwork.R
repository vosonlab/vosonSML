#' Create bimodal networks from social media data
#'
#' This function creates a directed and weighted bimodal network from a data frame of class \code{dataSource} (as 
#' created by the CollectData functions).
#'
#' @param datasource An object of class \code{datasource}. For Twitter data, it is also possible to provide a *list* 
#' of dataframes (i.e. dataframes that inherit class \code{dataSource} and \code{twitter}).
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
#' @export
CreateBimodalNetwork <- function(datasource, ...) {
  # searches the class list of datasource for matching method
  UseMethod("CreateBimodalNetwork", datasource)
}

# default function
CreateBimodalNetwork.default <- function(datasource, ...) {
  stop("Unknown social media data passed to create bimodal network.", call. = FALSE)
}
