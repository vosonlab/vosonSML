CreateDynamicNetwork.default <-
function(x,writeToFile)
  {
    if (missing(writeToFile)) {
      writeToFile <- FALSE # default = not write to file
    }
    cat("Error. Cannot operate on this data.\nUse the `CollectTemporalData` family of functions in the vosonSMLs package to collect temporal data for generating dynamic networks.\n")
    cat("Dynamic networks are currently supported for the following data sources:\n1. Facebook\n\n")
  }
