#' @title Web crawler authentication
#' 
#' @description Web crawler does not require authentication in this version of vosonSML.
#' 
#' @note Even though the web crawler does not require authentication in this version of vosonSML the \code{Authenticate} 
#' function must still be called to set the \code{socialmedia} identifier. This is used to route to the appropriate 
#' social media \code{Collect} function.
#' 
#' @param socialmedia Character string. Identifier for social media API to authenticate, set to \code{"web"}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return A \code{credential} object containing a \code{$auth = NULL} value and social media type descriptor 
#' \code{$socialmedia} set to \code{"web"}. Object has the class names \code{"credential"} and \code{"web"}.
#' 
#' @examples
#' \dontrun{
#' # web authentication
#' webAuth <- Authenticate("web")
#' }
#' 
#' @export
Authenticate.web <- function(socialmedia, ...) {
  # no web authentication required in this version
  credential <- list(socialmedia = "web", auth = NULL)
  class(credential) <- append(class(credential), c("credential", "web"))
  
  return(credential)
}
