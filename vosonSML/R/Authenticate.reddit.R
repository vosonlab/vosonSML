#' Reddit API authentication
#' 
#' @rdname Authenticate
#' @export
Authenticate.reddit <- function(socialmedia, ...) {
  # no reddit authentication required in this version
  credential <- list(socialmedia = "reddit", auth = "no authentication")
  class(credential) <- append(class(credential), list("credential", "reddit"))
  
  return(credential)
}
