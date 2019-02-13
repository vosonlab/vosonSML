#' Creates a semantic network from social media data
#'
#' This function creates a directed and weighted semantic network from a dataframe of class \code{datasource} (as 
#' created by the CollectData functions). These networks describe the semantic relationships between concepts. For 
#' example in twitter the concepts can be significant words or terms (hashtags) extracted from the text corpus of the 
#' data (tweets).
#'
#' @param datasource A dataframe of class \code{datasource} and class of a social media type.
#' @param type Character string. Type of network to be created.
#' @param ... Additional parameters to pass to the network creation method.
#' 
#' @param removeTermsOrHashtags Character string vector. Default is none. Otherwise this argument specifies which terms 
#' or hashtags (i.e. vertices with matching 'name') should be removed from the semantic network. This is useful to 
#' remove the search term or hashtag that was used to collect the data (i.e. remove the corresponding vertex in the 
#' graph). For example, a value of "#auspol" means that if there is a vertex with the name "#auspol" then this vertex 
#' will be removed.
#' @param stopwordsEnglish Logical. If \code{TRUE} then English stopwords are removed from the tweets (e.g. words such 
#' as 'the' or 'and'). Using \code{FALSE} may be helpful non-English data sets. The default is \code{TRUE} (i.e. 
#' stopwords will be removed).
#' @param termFreq Numeric integer. Specifies the percentage of most frequent TERMS to include. For example, a value 
#' of 20 means that the 20 percent most frequently occurring terms will be included in the semantic network. The 
#' default value is 5, meaning the 5 percent most frequent terms are used.
#' @param hashtagFreq Numeric integer. Specifies the percentage of most frequent HASHTAGS to include. For example, a 
#' value of 80 means that the 80 percent most frequently occurring hashtags will be included in the semantic network. 
#' The default value is 50, meaning the 50 percent most frequent hashtags are used. ** NOT IMPLEMENTED YET -**
#' ** DEFAULTS TO ALL HASHTAGS **.
#' @param writeToFile Logical. If \code{TRUE} then the network is saved to file in current working directory (graphml 
#' format), with filename denoting the current datetime and the type of network.
#' 
#' @note Supported data sources: \code{twitter}
#' 
#' @seealso \code{\link{Create}}
#' @keywords create semantic network twitter
#'
#' @noRd 
# @rdname Create.semantic
#' @method Create semantic
#' @export
Create.semantic <- function(datasource, type, ...) {
  UseMethod("Create.semantic", datasource)
}

#' @export
Create.semantic.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create semantic network.", call. = FALSE) 
}
