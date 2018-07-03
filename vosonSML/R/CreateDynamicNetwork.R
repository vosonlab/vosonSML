#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Create} function
#'
#' Create dynamic networks from social media data (networks that vary over
#' time)
#'
#' This function creates a dynamic network from social media data (i.e. from
#' data frames of class \code{dataSource} and class \code{temporal}).
#'
#' This function creates a directed network from a data frame of class
#' \code{dataSource} and \code{temporal} (which are created using the
#' `CollectTemporalData` family of functions in the vosonSML package).
#'
#' The resulting dynamic network is an igraph graph object. This graph object
#' is dynamic because all edges in the network have a timestamp attribute
#' expressing their existence as a function of time. Edges are directed and
#' non-weighted (i.e. for each 'interaction' between two vertices there exists
#' an edge with a timestamp attribute). For example, a Facebook user i may
#' 'comment on' post j at time T1 and T2, which is represented by two edges
#' with timestamp attribute T1 and T2, respectively.
#'
#' @param x a data frame of class \code{dataSource} and class \code{temporal}.
#' @param writeToFile logical. If \code{TRUE} then the network is saved to file
#' in current working directory (GRAPHML format), with filename denoting the
#' current date/time and the type of network.
#' @return An igraph graph object, with directed and non-weighted edges.
#' @note Not all data sources in vosonSML can be used for creating
#' dynamic networks.
#'
#' Currently supported data sources are:
#'
#' - Facebook
#'
#' There are three types of edge attributes for timestamp data. The
#' \code{timestamp} edge attribute is the timestamp in human readable format.
#' For example, "2015-05-29 19:54:39", in the format YYYY-MM-DD HH:MM:SS. The
#' \code{timestampNumeric} edge attribute utilises an 'epoch' starting from the
#' timestamp of the oldest comment in the data set, which is given the value 0
#' (zero). Therefore, the oldest comment in the dataset will become 0 (zero),
#' and all comments thereafter will be assigned a number that represents the
#' number of seconds each comment occurred *after* the 'epoch' comment (i.e.
#' from the zero starting time). The \code{timestampUnixEpoch} edge attribute
#' utilises the standard Unix epoch, indicating the number of seconds since
#' January 1, 1970 (i.e. how many seconds after the Unix epoch each commented
#' was posted). This provides the ability to compare different dynamic
#' datasets, perform unions on dynamic networks, etc.
#'
#' Dynamic networks created using Facebook data are bimodal. This means that
#' there are two types of vertices present in the network (i.e. Facebook users
#' and Facebook posts), with edges representing the time when user i commented
#' on post j. Currently timestamp data is not available through the Facebook
#' API for 'likes' data (i.e. when user i 'likes' post j), so relationships
#' based on 'likes' are excluded from the dynamic network.
#'
#' Other data sources (e.g. YouTube and Twitter) will be implemented in the
#' future. Additionally, the user is notified if they try to create dynamic
#' networks for incompatible data sources.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso See \code{CollectDataFacebook} to collect data for creating dynamic
#' networks in vosonSML.
#' @references Kolaczyk, E. D., Csardi, G. (2014). Statistical analysis of
#' network data with R. New York: Springer, Chapter 10.
#' @keywords SNA dynamic network igraph social media
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
#'   # Run the `CollectTemporalDataFacebook` function and
#'   # store the results in variable `myTemporalFacebookData`
#'   myTemporalFacebookData <- CollectTemporalDataFacebook(pageName="StarWars",
#'     rangeFrom="2015-05-14",rangeTo="2015-06-04",verbose=FALSE,writeToFile=FALSE)
#'
#'   # Create a dynamic 'bimodal' Facebook network using `CreateDynamicNetwork`
#'   g_bimodal_dynamic_facebook <- CreateDynamicNetwork(myTemporalFacebookData)
#'
#'   # View descriptive information about the bimodal network
#'   g_bimodal_facebook
#' }
#' @export
CreateDynamicNetwork <-
function(x,writeToFile)
 {
   if (missing(writeToFile)) {
     writeToFile <- FALSE # default = not write to file
   }
     UseMethod("CreateDynamicNetwork",x)
  }
