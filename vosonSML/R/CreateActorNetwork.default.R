CreateActorNetwork.default <-
function(x,writeToFile)
  {
    if (missing(writeToFile)) {
      writeToFile <- FALSE # default = not write to file
    }
      cat("Error. Cannot create actor network using this type of data (see help file for data types and sources).\n")
      if (inherits(x,"temporal")) {
        cat("(The data you supplied is temporal. Please use the `CreateDynamicNetwork` function for temporal data.)\n")
      }
  }
