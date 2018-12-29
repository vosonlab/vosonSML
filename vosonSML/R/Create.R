#' Create networks from social media data
#'
#' This function creates networks from social media data (i.e. from data frames of class \code{dataSource}. 
#' \code{Create} is the final step of the \code{Authenticate}, \code{Collect}, \code{Create} workflow. This function is
#' a convenient UI wrapper to the core create*Network family of functions.
#'
#' Note: when creating Twitter networks, the user information can be collected separately using the 
#' \code{\link{PopulateUserInfoTwitter}} function and stored into the network as vertex attributes (this involves 
#' additional calls to the Twitter API).
#'
#' @param dataSource a data frame of class \code{dataSource}
#' @param type character, type of network to be created, currently supports "actor", "bimodal", "dynamic", "semantic" 
#' and "ego"
#' @param ... additional parameters for create*Network functions
#' @return an igraph graph object
#' 
#' @author Chung-hong Chan <chainsawtiney@@gmail.com>
#' 
#' @examples
#' \dontrun{
#' require(magrittr)
#' 
#' ## instagram ego network example
#' 
#' my_app_id     <- "123456789098765"
#' my_app_secret <- "abc123abc123abc123abc123abc123ab"
#' my_usernames  <- c("senjohnmccain", "obama")
#'
#' my_ego_network <- Authenticate("instagram", appID = my_app_id, appSecret = my_app_secret) %>% 
#'   Collect(ego = TRUE, username = my_usernames) %>% Create
#'
#' ## youtube actor network example
#'
#' my_api_key   <- "314159265358979qwerty"
#' my_video_ids <- c("W2GZFeYGU3s","mL27TAJGlWc")
#'
#' my_actor_network <- Authenticate("youtube", apiKey = my_api_key) %>% 
#'   Collect(videoIDs = my_video_ids) %>% Create('actor')
#'
#' }
#' @export
Create <- function(dataSource, type = "actor", ...) {
  
  if (inherits(dataSource, "ego")) {
    return(CreateEgoNetworkFromData(dataSource)) ## you cannot create actor out of ego data
  }

  creator <- switch(tolower(type),
                    actor = CreateActorNetwork,
                    bimodal = CreateBimodalNetwork,
                    dynamic = CreateDynamicNetwork,
                    semantic = CreateSemanticNetwork,
                    ego = CreateEgoNetworkFromData,
                    stop("Unknown Type"))

  network_to_return <- creator(dataSource, ...)
  class(network_to_return) <- append(class(network_to_return), c("vosonSML"))

  return(network_to_return)
}
