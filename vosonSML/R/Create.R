#' Create networks from social media data
#'
#' This function creates networks from social media data (i.e. from data frames of class \code{dataSource}.
#' \code{Create} is the final step of the \code{Authenticate}, \code{Collect}, \code{Create} workflow. This function is
#' a convenient UI wrapper to the core Create*Network family of functions.
#'
#' Note: when creating Twitter networks, the user information can be collected separately using the
#' \code{\link{PopulateUserInfoTwitter}} function and stored into the network as vertex attributes (this involves
#' additional calls to the Twitter API).
#'
#' @param dataSource A data frame of class \code{dataSource}.
#' @param type Character string. Type of network to be created, can be \code{"actor"}, \code{"bimodal"},
#' \code{"dynamic"}, \code{"semantic"} or \code{"ego"}.
#' @param ... Additional parameters for network creation for appropriate \code{socialmedia} and network \code{type}.
#' Refer to Create*Network functions for more details.
#' \describe{
#'   \item{twitter|actor:}{\code{[writeToFile]}}
#'   \item{youtube|actor:}{\code{[writeToFile]}}
#'   \item{reddit|actor:}{\code{[weightEdges, includeTextData, cleanText, writeToFile]}}
#'   \item{twitter|bimodal:}{\code{[writeToFile, removeTermsOrHashtags]}}
#'   \item{instagram|bimodal:}{\code{[writeToFile, ...]}}
#'   \item{facebook|bimodal:}{\code{[writeToFile, removeTermsOrHashtags, ...]}}
#'   \item{facebook|dynamic:}{\code{[writeToFile]}}
#'   \item{twitter|semantic:}{\code{[writeToFile, termFreq, hashtagFreq, removeTermsOrHashtags,}\cr
#'                            \code{stopwordsEnglish]}}
#'   \item{instagram|ego:}{[writeToFile]}
#' }
#'
#' @return An \code{igraph} graph object.
#'
#' @seealso \code{\link{Authenticate}}, \code{\link{Collect}}
#' @keywords create actor bimodal dynamic semantic ego twitter youtube instagram facebook
#'
#' @examples
#' \dontrun{
#' require(magrittr)
#'
#' ## youtube actor network example
#'
#' myYoutubeAPIKey <- "xxxxxxxxxxxxxxxxxxxxxx"
#' listYoutubeVideoIDs <- c("W2GZFeYGU3s", "mL27TAJGlWc")
#'
#' myActorNetwork <- Authenticate("youtube", apiKey = myYoutubeAPIKey) %>%
#'   Collect(videoIDs = listYoutubeVideoIDs) %>% Create("actor")
#'
#' ## instagram ego network example
#'
#' myInstaAppID <- "xxxxxxxxxxx"
#' myInstaAppSecret <- "xxxxxxxxxxxxxxxxxxxxxx"
#' listInstaUsernames <- c("senjohnmccain", "obama")
#'
#' myEgoNetwork <- Authenticate("instagram", appID = myInstaAppID, appSecret = myInstaAppSecret) %>%
#'   Collect(ego = TRUE, username = listInstaUsernames) %>% Create("ego")
#' }
#'
#' @export
Create <- function(dataSource, type = "actor", ...) {
  if (inherits(dataSource, "ego")) {
    return(CreateEgoNetworkFromData(dataSource)) # you cannot create actor out of ego data
  }
  
  creator <- switch(tolower(type),
                    actor = CreateActorNetwork,
                    bimodal = CreateBimodalNetwork,
                    dynamic = CreateDynamicNetwork,
                    semantic = CreateSemanticNetwork,
                    ego = CreateEgoNetworkFromData,
                    stop("Unknown Type"))
  
  networkToReturn <- creator(dataSource, ...)
  class(networkToReturn) <- append(class(networkToReturn), c("vosonSML"))
  
  return(networkToReturn)
}
