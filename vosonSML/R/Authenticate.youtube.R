#' Youtube API authentication
#'
#' @rdname Authenticate
#' @export
Authenticate.youtube <- function(socialmedia, apiKey, ...) {
  if (missing(apiKey)) {
    stop("Missing youtube API key.", call. = FALSE)
  }
  
  credential <- list(socialmedia = "youtube", auth = apiKey)
  class(credential) <- append(class(credential), c("credential", "youtube"))
  return(credential)
}
