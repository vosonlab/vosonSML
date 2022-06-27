#' @title Create a credential object to access social media APIs
#'
#' @description \code{Authenticate} creates a \code{credential} object that enables R to make authenticated calls to
#' social media APIs. A \code{credential} object is a S3 object containing authentication related information such as an
#' access token or key, and a class name identifying the social media that grants authentication. \code{Authenticate} is
#' the first step of the \code{Authenticate}, \code{\link{Collect}} and \code{\link{Create}} workflow.
#'
#' Refer to \code{\link{Authenticate.twitter}}, \code{\link{Authenticate.youtube}} and
#' \code{\link{Authenticate.reddit}}, \code{\link{Authenticate.web}} for parameters and usage.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate with. Supported social media
#' are \code{"twitter"}, \code{"youtube"}, \code{"reddit"} and \code{"web"}.
#' @param ... Optional parameters to pass to functions providied by supporting R packages that are used for social media
#' API access.
#'
#' @export
Authenticate <- function(socialmedia, ...) {
  # searches the class list of socialmedia for matching method
  UseMethod("Authenticate", socialmedia)
}

# default function used as proxy method dispatch
#' @export
Authenticate.default <- function(socialmedia, ...) {
  # check if social media type is a character string
  if (!is.character(socialmedia)) {
    stop("Authentication social media type should be a character string.",
         call. = FALSE)
  }

  # check if function exists for social media type
  # todo: perhaps search authenticate methods so this can be extensible
  func_name <- paste0("Authenticate", ".", socialmedia)
  if (!exists(func_name, where = asNamespace("vosonSML"), mode = "function")) {
    stop("Unknown social media type passed to authenticate.", call. = FALSE)
  }

  # add social media type to value class list
  class(socialmedia) <- append(class(socialmedia), socialmedia)

  # call authenticate
  Authenticate(socialmedia, ...)
}
