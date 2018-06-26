#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Authenticate} function
#'
#' YouTube API Authentication
#'
#' OAuth based authentication with the Google API
#'
#' In order to collect data from YouTube, the user must first authenticate with
#' Google's Application Programming Interface (API). Users can obtain a Google
#' Developer API key at: https://console.developers.google.com
#'
#' @param apiKeyYoutube character string specifying your Google Developer API
#' key.
#' @return This is called for its side effect.
#' @note In the future this function will enable users to save the API key in
#' working directory, and the function will automatically look for a locally
#' stored key whenever it is called without \code{apiKeyYoutube} argument.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithFacebookAPI} and
#' \code{AuthenticateWithTwitterAPI} for other ways to collect social media
#' data.
#' @keywords youtube social media SNA
#' @examples
#'
#' \dontrun{
#'   # Replace with your Google Developer API Key:
#'   my_apiKeyYoutube <- "314159265358979qwerty"
#'
#'   apiKeyYoutube <- AuthenticateWithYoutubeAPI(my_apiKeyYoutube)
#' }
#' @export
AuthenticateWithYoutubeAPI <-
function(apiKeyYoutube) {

  return(apiKeyYoutube)

}
