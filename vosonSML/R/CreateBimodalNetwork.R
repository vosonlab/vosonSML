#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Create} function
#'
#' Create bimodal networks from social media data
#'
#' This function creates a bimodal network from social media data (i.e. from
#' data frames of class \code{dataSource}, or for Twitter data it is also
#' possible to provide a *list* of data frames), with edges representing
#' relationships between actors of two different types (e.g. Facebook users and
#' Facebook posts, with edges representing whether a user has commented or
#' 'liked' a post).
#'
#' This function creates a (directed and weighted) bimodal network from a data
#' frame of class \code{dataSource} (which are created using the `CollectData`
#' family of functions in the vosonSML package), or a *list* of Twitter
#' data frames collected using \code{CollectDataTwitter} function.
#'
#' The resulting network is an igraph graph object. This graph object is
#' bimodal because edges represent relationships between vertices of two
#' different types. For example, in a bimodal Facebook network, vertices
#' represent Facebook users or Facebook posts, and edges represent whether a
#' user has commented or 'liked' a post. Edges are directed and weighted (e.g.
#' if user i has commented n times on post j, then the weight of this directed
#' edge equals n).
#'
#' @param x a data frame of class \code{dataSource}. For Twitter data, it is
#' also possible to provide a *list* of data frames (i.e. data frames that
#' inherit class \code{dataSource} and \code{twitter}). Only lists of Twitter
#' data frames are supported at this time. If a list of data frames is
#' provided, then the function binds these row-wise and computes over the
#' entire data set.
#' @param writeToFile logical. If \code{TRUE} then the network is saved to file
#' in current working directory (GRAPHML format), with filename denoting the
#' current date/time and the type of network.
#' @param removeTermsOrHashtags character vector. Default is none. Otherwise
#' this argument specifies which terms or hashtags (i.e. vertices with matching
#' `name`) should be removed from the bimodal network. This is useful to remove
#' the search term or hashtag that was used to collect the data (i.e. remove
#' the corresponding vertex in the graph). For example, a value of "#auspol"
#' means that if there is a vertex with the exact name "#auspol" then this
#' vertex will be removed.
#' @return An igraph graph object, with weighted and directed edges.
#' @note Not all data sources in vosonSML can be used for creating
#' bimodal networks.
#'
#' Currently supported data sources are:
#'
#' - Facebook - Twitter
#'
#' Other data sources (e.g. YouTube) will be implemented in the future.
#' Additionally, the user is notified if they try to create bimodal networks
#' for incompatible data sources.
#'
#' For Twitter data, bimodal networks can be created from multiple data frames
#' (i.e. datasets collected individually using CollectDataTwitter). Simply
#' create a list of the data frames that you wish to create a network from. For
#' example, \code{myList <- list(myTwitterData1, myTwitterData2,
#' myTwitterData3)}.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso See \code{CollectDataFacebook} and \code{CollectDataTwitter} to
#' collect data for creating bimodal networks in vosonSML.
#' @keywords SNA bimodal network igraph social media
#' @examples
#'
#' \dontrun{
#'   ## This example shows how to collect Facebook page data and create a bimodal network
#'
#'   # Use your own values for myAppID and myAppSecret
#'   myAppID <- "123456789098765"
#'   myAppSecret <- "abc123abc123abc123abc123abc123ab"
#'
#'   # Authenticate with the Facebook API using `AuthenticateWithFacebookAPI`
#'   fb_oauth <- AuthenticateWithFacebookAPI(appID=myAppID, appSecret=myAppSecret,
#'     extended_permissions=FALSE, useCachedToken=TRUE)
#'
#'   # Run the `CollectDataFacebook` function and store the results in variable `myFacebookData`
#'   myFacebookData <- CollectDataFacebook(pageName="StarWars", rangeFrom="2014-05-15",
#'   rangeTo="2014-06-03",writeToFile=FALSE,verbose=TRUE)
#'
#'   # Create a 'bimodal' network using \code{CreateBimodalNetwork}
#'   g_bimodal_facebook <- CreateBimodalNetwork(myFacebookData)
#'
#'   # View descriptive information about the bimodal network
#'   g_bimodal_facebook
#' }
#'
CreateBimodalNetwork <-
function(x,writeToFile,removeTermsOrHashtags)
 {
   if (missing(writeToFile)) {
     writeToFile <- FALSE # default = not write to file
   }
   if (!missing(removeTermsOrHashtags)) {
     removeTermsOrHashtags <- as.vector(removeTermsOrHashtags) #coerce to vector... to be sure
   }

   if (missing(removeTermsOrHashtags)) {
     removeTermsOrHashtags <- "foobar"
   }
    UseMethod("CreateBimodalNetwork",x)
  }
