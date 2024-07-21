#' @title Collect data from social media for generating networks
#'
#' @description This function collects data from social media and structures it into a dataframe that can be used for
#'   creating networks for further analysis. \code{Collect} is the second step of the \code{\link{Authenticate}},
#'   \code{Collect}, and \code{\link{Create}} workflow.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate}.
#' @param ... Optional parameters to pass to functions providied by supporting R packages that are used for social media
#'   API collection.
#' @param writeToFile Logical. Write data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information. Default is \code{TRUE}.
#' 
#' @export
Collect <- function(credential, ..., writeToFile = FALSE, verbose = TRUE) {
  endpoint <- check_dots("endpoint", ...)

  if (!is.null(endpoint)) {
    class(endpoint) <- append(class(endpoint), endpoint)
    UseMethod("Collect", endpoint)
  } else {
    UseMethod("Collect", credential)
  }
}

#' @export
Collect.default <- function(credential, ..., writeToFile = FALSE, verbose = TRUE) {
  stop("Unknown credential social media or endpoint passed to collect.", call. = FALSE)
}

#' @title Collect search data
#'
#' @noRd
#' @method Collect search
#' @export
Collect.search <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  UseMethod("Collect.search", credential)
}

#' @noRd
#' @export
Collect.search.default <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  stop("Unknown social media credential passed to collect search data", call. = FALSE)
}

#' @title Collect timeline data
#'
#' @noRd
#' @method Collect timeline
#' @export
Collect.timeline <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  UseMethod("Collect.timeline", credential)
}

#' @noRd
#' @export
Collect.timeline.default <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  stop("Unknown social media credential passed to collect timeline data", call. = FALSE)
}

#' @noRd
#' @method Collect reddit
#' @export
Collect.reddit <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  UseMethod("Collect.thread", credential)
}

#' @title Collect thread data
#'
#' @noRd
#' @method Collect thread
#' @export
Collect.thread <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  UseMethod("Collect.thread", credential)
}

#' @noRd
#' @export
Collect.thread.default <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  stop("Unknown social media credential passed to collect thread data", call. = FALSE)
}

#' @title Collect list data
#'
#' @noRd
#' @method Collect listing
#' @export
Collect.listing <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  UseMethod("Collect.listing", credential)
}

#' @noRd
#' @export
Collect.listing.default <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  stop("Unknown social media credential passed to collect listing data", call. = FALSE)
}

#' @noRd
#' @method Collect mastodon
#' @export
Collect.mastodon <- function(credential, endpoint, ..., writeToFile = FALSE, verbose = TRUE) {
  UseMethod("Collect.search", credential)
}
