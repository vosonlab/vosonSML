CreateBimodalNetwork.default <-
function(x,writeToFile,removeTermsOrHashtags, ...)
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
      cat("Error. Cannot create bimodal network using this type of data (see help file for data types and sources).\n")
      # if (inherits(x,"temporal")) {
      #   cat("(The data you supplied is temporal. Please use the `CreateDynamicNetwork` function for temporal data.)\n")
      # }
  }
