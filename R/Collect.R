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
  timer_pkg <- FALSE
  if (requireNamespace("tictoc", quietly = TRUE)) {
    timer_pkg <- TRUE
  }

  # set the environment encoding to UTF-8 for data collection
  saved_enc <- getOption("encoding")
  saved_ua <- getOption("HTTPUserAgent")
  on.exit({
    if (timer_pkg) {
      tictoc::toc(quiet = FALSE, func.toc = format_toc)
    }
    options(encoding = saved_enc)
    options(HTTPUserAgent = saved_ua)
  }, add = TRUE)
  options(encoding = "UTF-8")
  options(HTTPUserAgent = paste0("vosonSML v.", get_version(), " (R Package)"))
  if (timer_pkg) {
    tictoc::tic(msg = "Elapsed time")
  }

  # searches the class list of credential for matching method
  UseMethod("Collect", credential)
}

# default function
#' @export
Collect.default <- function(credential, ...) {
  stop("Unknown social media type passed to collect.", call. = FALSE)
}
