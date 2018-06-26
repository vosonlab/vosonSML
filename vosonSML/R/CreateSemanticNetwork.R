#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Create} function
#'
#' Create semantic networks from social media data (semantic relationships
#' between concepts)
#'
#' This function creates a semantic network from social media data (i.e. from
#' data frames of class \code{dataSource}, or for Twitter data it is also
#' possible to provide a list of data frames). In such semantic networks,
#' concepts are words/terms extracted from the text corpus of social media data
#' (e.g. tweets on Twitter).
#'
#' This function creates a weighted network from a data frame of class
#' \code{dataSource} (which are created using the `CollectData` family of
#' functions in the vosonSML package), or a list of Twitter data frames
#' collected using \code{CollectDataTwitter} function.
#'
#' The resulting semantic network is an igraph graph object. This graph object
#' is semantic because vertices represent unique concepts (in this case unique
#' terms/words extracted from a social media text corpus), and edges represent
#' the co-occurrence of terms for all observations in the data set. For
#' example, for a Twitter semantic network, vertices represent either hashtags
#' (e.g. "#auspol") or single terms ("politics"). If there are 1500 tweets in
#' the data set (i.e. 1500 observations), and the term "#auspol" and the term
#' "politics" appear together in every tweet, then this will be represented by
#' an edge with weight equal to 1500.
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
#' @param termFreq numeric integer, specifying the percentage of most frequent
#' TERMS to include. For example, a value of 20 means that the 20 percent most
#' frequently occurring terms will be included in the semantic network. The
#' default value is 5, meaning the 5 percent most frequent terms are used.
#' @param hashtagFreq ** NOT IMPLEMENTED YET - DEFAULTS TO ALL HASHTAGS **.
#' numeric integer, specifying the percentage of most frequent HASHTAGS to
#' include. For example, a value of 80 means that the 80 percent most frequently
#' occurring hashtags will be included in the semantic network. The default
#' value is 50, meaning the 50 percent most frequent hashtags are used.
#' @param removeTermsOrHashtags character vector. Default is none. Otherwise
#' this argument specifies which terms or hashtags (i.e. vertices with matching
#' `name`) should be removed from the semantic network. This is useful to
#' remove the search term or hashtag that was used to collect the data (i.e.
#' remove the corresponding vertex in the graph). For example, a value of
#' "#auspol" means that if there is a vertex with the name "#auspol" then this
#' vertex will be removed.
#' @param stopwordsEnglish logical. If \code{TRUE} then English stopwords are
#' removed from the tweets (e.g. words such as 'the' or 'and'). Using
#' \code{FALSE} may be helpful non-English data sets. The default is
#' \code{TRUE} (i.e. stopwords will be removed).
#' @return An igraph graph object, with weighted edges.
#' @note Not all data sources in vosonSML can be used for creating
#' semantic networks.
#'
#' Currently supported data sources are:
#'
#' - Twitter
#'
#' Other data sources (e.g. YouTube and Facebook) will be implemented in the
#' future. Additionally, the user is notified if they try to create semantic
#' networks for incompatible data sources.
#'
#' For Twitter data, semantic networks can be created from multiple data frames
#' (i.e. datasets collected individually using CollectDataTwitter). Simply
#' create a list of the data frames that you wish to create a network from. For
#' example, \code{myList <- list(myTwitterData1, myTwitterData2,
#' myTwitterData3)}.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso See \code{CollectDataTwitter} to collect data for creating semantic
#' networks in vosonSML.
#' @keywords SNA semantic network igraph social media
#' @examples
#'
#' \dontrun{
#'   ## This example shows how to collect Twitter data and create a semantic network
#'
#'   # Firstly specify your API credentials
#'   my_api_key <- "1234567890qwerty"
#'   my_api_secret <- "1234567890qwerty"
#'   my_access_token <- "1234567890qwerty"
#'   my_access_token_secret <- "1234567890qwerty"
#'
#'   # Authenticate with the Twitter API using \code{AuthenticateWithTwitterAPI}
#'   AuthenticateWithTwitterAPI(api_key=my_api_key, api_secret=my_api_secret,
#'     access_token=my_access_token, access_token_secret=my_access_token_secret)
#'
#'   # Collect tweets data using \code{myTwitterData}
#'   myTwitterData <- CollectDataTwitter(searchTerm="#auspol",
#'     numTweets=200,writeToFile=FALSE,verbose=FALSE)
#'
#'   # Create a 'semantic' network using \code{CreateSemanticNetwork}
#'   g_semantic_twitter <- CreateSemanticNetwork(myTwitterData,writeToFile=FALSE,
#'     termFreq=20,hashtagFreq=80)
#' }
#'
CreateSemanticNetwork <-
function(x,writeToFile,termFreq,hashtagFreq,removeTermsOrHashtags,stopwordsEnglish)
  {
    if (missing(writeToFile)) {
      writeToFile <- FALSE # default = not write to file
    }
    if (missing(termFreq)) {
      termFreq <- 5 # default to the top 5% most frequent terms. reduces size of graph.
    }
    if (missing(hashtagFreq)) {
      hashtagFreq <- 50 # default to the top 50% hashtags. reduces size of graph. hashtags are 50% because they are much less frequent than terms.
    }
    if (missing(removeTermsOrHashtags)) {
      removeTermsOrHashtags <- NA
    }
    if (missing(stopwordsEnglish)) {
      stopwordsEnglish <- TRUE # default to true, because most English users will probably want this
    }
      UseMethod("CreateSemanticNetwork",x)
   }
