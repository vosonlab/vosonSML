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
#' \code{\link{AddUserData.twitter}} function. Additional calls can be made to the twitter API to get information
#' about users that were identified as nodes during network creation.
#' 
#' @seealso \code{\link{Create.actor}}, \code{\link{Create.bimodal}}, \code{\link{Create.semantic}}
#' @keywords create actor bimodal semantic network
#'
#' @export
Create <- function(datasource, type, ...) {
  # searches the class list of datasource for matching method
  UseMethod("Create", type)
}

#' @export
Create.default <- function(datasource, type, ...) {
  # check if network type is a character string
  if (!is.character(type)) {
    stop("Create network type should be a character string.", call. = FALSE) 
  }
  
  # check if function exists for network type
  # todo: perhaps search create methods so this can be extensible
  func_name <- paste0("Create", ".", type)
  if (!exists(func_name, where = asNamespace("vosonSML"), mode = "function")) {
    stop("Unknown network type passed to create.", call. = FALSE) 
  }
  
  # add social media type to value class list
  class(type) <- append(class(type), type)
  
  # call authenticate
  Create(datasource, type, ...)
}
