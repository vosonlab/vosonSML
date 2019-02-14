#' @title Create networks from social media data
#'
#' @description This function creates networks from social media data as produced from \code{\link{Collect}}. 
#' \code{Create} is the final step of the \code{\link{Authenticate}}, \code{\link{Collect}} and \code{Create} workflow.
#' 
#' There are three types of networks that can be created from collected data: \code{actor}, \code{bimodal} or 
#' \code{semantic}.
#' 
#' For \code{actor} networks refer to \code{\link{Create.actor.twitter}}, \code{\link{Create.actor.youtube}} and 
#' \code{\link{Create.actor.reddit}} for parameters and usage.
#' 
#' For \code{bimodal} and \code{semantic} networks refer to \code{\link{Create.bimodal.twitter}} and 
#' \code{\link{Create.semantic.twitter}} functions for parameters and usage respectively. 
#'
#' @param datasource Collected social media data of class \code{"datasource"} and \code{socialmedia}.
#' @param type Character string. Type of network to be created, can be \code{"actor"}, \code{"bimodal"} or 
#' \code{"semantic"}.
#' @param ... Optional parameters to pass to functions providied by supporting R packages that are used for social 
#' media network creation.
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
