#' Create actor network from social media data
#'
#' @param datasource Collected social media data with \code{datasource} and \code{socialmedia} class attributes.
#' @param ... Additional parameters to pass to the network creation method.
#' 
#' @param writeToFile Logical. Save network data to a file in the current working directory. Default is \code{FALSE}.
#'
#' @note Supported \code{datasource} \code{socialmedia}: \code{twitter}, \code{youtube}, \code{reddit}
#'
#' @seealso \code{\link{Create}}
#' @keywords create actor network twitter youtube reddit
#'
#' @export
CreateActorNetwork <- function(datasource, ...) {
  # searches the class list of data for matching method
  UseMethod("CreateActorNetwork", datasource)
}

# default function
#' @export
CreateActorNetwork.default <- function(datasource, ...) {
  stop("Unknown social media data passed to create actor network.", call. = FALSE)
}
