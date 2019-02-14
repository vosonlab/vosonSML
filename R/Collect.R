#' @title Collect data from social media for generating networks
#'
#' @description This function collects data from social media and structures it into a dataframe that can be used for 
#' creating networks for further analysis. \code{Collect} is the second step of the \code{\link{Authenticate}}, 
#' \code{Collect}, and \code{\link{Create}} workflow.
#'
#' Refer to \code{\link{Collect.twitter}}, \code{\link{Collect.youtube}} and 
#' \code{\link{Collect.reddit}} for parameters and usage.
#' 
#' @param credential A \code{credential} object generated from \code{Authenticate}.
#' @param ... Optional parameters to pass to functions providied by supporting R packages that are used for social media 
#' API collection.
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
