#' YouTube API Authentication
#'
#' OAuth based authentication with the Google API.
#'
#' In order to collect data from YouTube, the user must first authenticate with Google's Application Programming 
#' Interface (API). Users can obtain a Google Developer API key at: https://console.developers.google.com.
#'
#' @param apiKeyYoutube character string specifying your Google Developer API key.
#' 
#' @return This is called for its side effect.
#' 
#' @note In the future this function will enable users to save the API key in working directory, and the function will 
#' automatically look for a locally stored key whenever it is called without apiKeyYoutube argument.
#'
#' @noRd
AuthenticateWithYoutubeAPI <- function(apiKeyYoutube) {
  return(apiKeyYoutube)
}
