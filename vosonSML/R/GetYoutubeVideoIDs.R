#' Extract/scrape the IDs from a set of YouTube video URLs
#' 
#' This function reads a list of YouTube video URLs from a text file and
#' converts them to a vector object. For example,
#' "https://www.youtube.com/watch?v=73I5dRucCds" has the ID "73I5dRucCds". This
#' function can be used to create an object for the argument \code{videoIDs} in
#' the function \code{CollectDataYoutube}, that is, by extracting the IDs for a
#' set of YouTube videos and compiling them into a vector, ready for collecting
#' data with \code{CollectDataYoutube}.
#' 
#' 
#' @param file The connection to read from. This can be a local file, or a http
#' or ftp connection. It can also be a character string with the file name or
#' URI. The file must be plain text format with the URL of each YouTube video
#' specified on a new line (separated by character return). For example, the
#' first line might contain https://www.youtube.com/watch?v=73I5dRucCds, and
#' the second line might contain https://www.youtube.com/watch?v=6S9r_YbqHy8.
#' @return a character vector representing a set of YouTube video IDs, each
#' with number of characters equal to 11 (e.g. "73I5dRucCds").
#' @note This function is useful for lots of videos. However, many videos may
#' take a *long* time to collect data from. In such cases it is recommended to
#' use the \code{verbose=TRUE} argument for the function
#' \code{CollectDataYoutube}, in order to keep track of progress during
#' computation.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso Use \code{CollectDataYoutube} for collecting YouTube comments data.
#' @keywords youtube scraping vosonSML
#' @examples
#' 
#' \dontrun{
#'   ## This example shows how to use `GetYoutubeVideoIDs` to extract video IDs from YouTube
#'   ## video URLs, and then collect data using the function `CollectDataYoutube`
#' 
#'   # Use your own Google Developer API Key here:
#'   myApiKey <- "1234567890"
#' 
#'   # Authenticate with the Google API
#'   apiKeyYoutube <- AuthenticateWithYoutubeAPI(apiKeyYoutube=myApiKey)
#' 
#'   # Use the function `GetYoutubeVideoIDs` to automatically generate vector of IDs from
#'   # a plain text file of video URLs
#'   videoIDs <- GetYoutubeVideoIDs(file="youtube_to_scrape.txt")
#' 
#'   # Collect the data using function `CollectDataYoutube`
#'   myYoutubeData <- CollectDataYoutube(videoIDs,apiKeyYoutube,writeToFile=FALSE)
#' }
#' @export
GetYoutubeVideoIDs <-
function(file){

  videoIDsTemp <- read.table(file,
                 sep="\n",
                 strip.white=TRUE) # in case of user input error

  videoIDsTemp <- as.vector(videoIDsTemp$V1)

  videoIDsOut <- substr(videoIDsTemp,33,43)

}
