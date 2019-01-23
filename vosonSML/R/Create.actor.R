#' Create actor network from social media data
#'
#' @param datasource Collected social media data with \code{datasource} and \code{socialmedia} class attributes.
#' @param type Character string. Type of network to be created.
#' @param ... Additional parameters to pass to the network creation method.
#' 
#' @param writeToFile Logical. Save network data to a file in the current working directory. Default is \code{FALSE}.
#'
#' @note Supported data sources: \code{twitter}, \code{youtube}, \code{reddit}
#'
#' @seealso \code{\link{Create}}
#' @keywords create actor network twitter youtube reddit
#' 
#' @rdname Create.actor
#' @method Create actor
#' @export
Create.actor <- function(datasource, type, ...) {
  UseMethod("Create.actor", datasource)
}

#' @export
Create.actor.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create.", call. = FALSE) 
}
