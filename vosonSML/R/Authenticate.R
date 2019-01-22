#' Create a credential to access social media APIs
#'
#' \code{Authenticate} creates a \code{credential} object that enables R to make authenticated calls to social media
#' APIs. A \code{credential} object is a S3 object with the authentication-related information such as access tokens
#' and the information on the social media that grant authentication. \code{Authenticate} is the first step of the
#' \code{Authenticate}, \code{\link{Collect}} and \code{\link{Create}} workflow.
#'
#' Twitter authentication uses OAuth and requires Application-user authentication API keys as described here: 
#' \url{https://developer.twitter.com/en/docs/basics/authentication/overview/oauth}
#' 
#' Youtube authentication uses OAuth2 and requires a Google Developer API key as described here:
#' \url{https://developers.google.com/youtube/v3/docs/}
#' 
#' Reddit does not require authentication in this version of vosonSML.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate with.\cr
#' Supports: \code{"twitter"}, \code{"youtube"}, \code{"reddit"}.
#' @param ... Additional parameters for authentication appropriate to \code{socialmedia} identifier.
#' 
#' @param apiKey Character string. Specifies the app API key used for authentication.
#' 
#' @return A \code{credential} object with authentication information.
#'
#' @seealso \code{\link{Collect}}, \code{\link{Create}}
#' @keywords authenticate credential twitter youtube reddit
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
    stop("Authentication social media type should be a character string.", call. = FALSE) 
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
