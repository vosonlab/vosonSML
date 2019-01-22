#' Reddit API authentication
#' 
#' @rdname Authenticate
#' @export
Authenticate.reddit <- function(socialmedia, ...) {
  # no reddit authentication required in this version
  credential <- list(socialmedia = "reddit", auth = NULL)
  class(credential) <- append(class(credential), c("credential", "reddit"))
  
  return(credential)
}
