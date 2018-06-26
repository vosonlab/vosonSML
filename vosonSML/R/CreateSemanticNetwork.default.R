CreateSemanticNetwork.default <-
function(x,writeToFile,termFreq,hashtagFreq,removeTermsOrHashtags)
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
      cat("Error. Cannot create semantic network using this type of data (see help file for data types and sources).\n")
      if (inherits(x,"temporal")) {
        cat("(The data you supplied is temporal. Please use the `CreateDynamicNetwork` function for temporal data.)\n")
      }
  }
