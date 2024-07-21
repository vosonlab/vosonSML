#' @title YouTube API authentication
#'
#' @description YouTube authentication uses OAuth2 and requires a Google Developer API key as described here:
#'   \url{https://developers.google.com/youtube/v3/docs/}.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate, set to \code{"youtube"}.
#' @param apiKey Character string. Google developer API key to authenticate.
#' @param ... Additional parameters passed to function. Not used in this method.
#' @param verbose Logical. Output additional information. Default is \code{TRUE}.
#' 
#' @return A \code{credential} object containing an api key \code{$auth} and social media type descriptor
#'   \code{$socialmedia} set to \code{"youtube"}. Object has the class names \code{"credential"} and \code{"youtube"}.
#'
#' @examples
#' \dontrun{
#' # youtube authentication with google developer api key
#' myAPIKey <- "xxxxxxxxxxxx"
#'
#' youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
#' }
#'
#' @export
Authenticate.youtube <- function(socialmedia, apiKey, ..., verbose = TRUE) {
  
  if (missing(apiKey)) stop("Missing YouTube API key.", call. = FALSE)

  apiKey <- check_chr(apiKey, param = "YouTube API key")

  credential <- list(socialmedia = "youtube", auth = apiKey)
  class(credential) <- append(class(credential), c("credential", "youtube"))

  credential
}
