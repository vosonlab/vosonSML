#' @title Reddit API authentication
#'
#' @description Reddit does not require authentication in this version of vosonSML.
#'
#' @note Even though reddit does not require authentication in this version of vosonSML the \code{Authenticate} function
#'   must still be called to set the \code{socialmedia} identifier. This is used to route to the appropriate social
#'   media \code{Collect} function.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate, set to \code{"reddit"}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A \code{credential} object containing a \code{$auth = NULL} value and social media type descriptor
#'   \code{$socialmedia} set to \code{"reddit"}. Object has the class names \code{"credential"} and \code{"reddit"}.
#'
#' @examples
#' \dontrun{
#' # reddit authentication
#' redditAuth <- Authenticate("reddit")
#' }
#'
#' @export
Authenticate.reddit <- function(socialmedia, ...) {
  # no reddit authentication required in this version
  credential <- list(socialmedia = "reddit", auth = NULL)
  class(credential) <-
    append(class(credential), c("credential", "reddit"))

  return(credential)
}
