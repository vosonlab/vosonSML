#' Collect data from social media for generating networks
#'
#' This function collects data from social media APIs, and structures the data into a data frame of class
#' \code{dataSource}, ready for creating networks for further analysis. \code{Collect} is the second step of the
#' \code{Authenticate}, \code{Collect}, \code{Create} workflow. This function is a convenient UI wrapper to the core
#' CollectDataFrom* family of functions.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate}.
#' @param ... Additional parameters for data collection by appropriate to credential \code{socialmedia} type.
#'
#' @param writeToFile Logical. If the collected data should be written to file. Default is \code{FALSE}.
#' @param verbose Logical. Outputs additional information about the data collection. Default is \code{FALSE}.
#' 
#' @return A data.frame object of class \code{dataSource} that can be used with \code{Create}.
#'
#' @seealso \code{\link{Authenticate}}, \code{\link{Create}}
#' @keywords collect twitter youtube reddit
#'
#' @export
Collect <- function(credential, ...) {
  # searches the class list of credential for matching method
  UseMethod("Collect", credential)
}

# default function
#' @export
Collect.default <- function(credential, ...) {
  stop("Unknown social media type passed to collect.", call. = FALSE) 
}
