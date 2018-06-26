CreateDynamicNetwork.facebook <-
function(x,writeToFile)
{

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  dataCombinedUNIQUE <- x # match the variable names (this must be used to avoid warnings in package compilation)

  # if `dataCombinedUNIQUE` is a list of dataframes, then need to convert these into one dataframe
  suppressWarnings(
    if (class(dataCombinedUNIQUE)=="list") {
    dataCombinedUNIQUE <- do.call("rbind", dataCombinedUNIQUE)
    }
  )

  cat("\nCreating Facebook dynamic network...\n")

  # need to convert the timestamps to integer values.
  # we have two 'epochs' to work with.
  # our first 'epoch' will start from the timestamp of OLDEST comment, which will become 0 (zero).
  # our second 'epoch' will utilise the unix epoch, i.e. number of seconds since Jan 1 1970.
  # both of these 'epoch' edge attributes are useful in different ways.

  dataCombinedUNIQUE$commentTimestamp <- as.character(strptime(dataCombinedUNIQUE$commentTimestamp, "%Y-%m-%dT%H:%M:%S+0000"))

  # THIS GIVES US THE NUMBER OF SECONDS SINCE JANUARY 1, 1970:
  dataCombinedUNIQUE$commentTimestampUnixEpoch <- as.numeric(as.POSIXct(dataCombinedUNIQUE$commentTimestamp))

  tempVar <- strptime(dataCombinedUNIQUE$commentTimestamp, "%Y-%m-%dT%H:%M:%S+0000")

  tempVarConverted <- as.numeric(tempVar - min(tempVar))

  dataCombinedUNIQUE$commentTimestampConverted <- tempVarConverted

  # make a dataframe of the relations between actors
  # we need a dataframe here because igraph needs it AFAIK
  relations <- data.frame(from=dataCombinedUNIQUE$from,to=dataCombinedUNIQUE$to,edgeType=dataCombinedUNIQUE$edgeType,timestamp=dataCombinedUNIQUE$commentTimestamp,timestampNumeric=dataCombinedUNIQUE$commentTimestampConverted,timestampUnixEpoch=dataCombinedUNIQUE$commentTimestampUnixEpoch)
  toDel <- which(relations$edgeType=="Like")
  relations <- relations[-toDel,]

  # make a vector of all the unique actors in the network1
  usersVec <- rep(c("User"),length(unique(relations$from)))
  postsVec <- rep(c("Post"),length(unique(relations$to)))
  usersAndPostsVec <- c(usersVec,postsVec)
  actors <- data.frame(name=unique(factor(c(as.character(unique(relations$from)),as.character(unique(relations$to))))),type=usersAndPostsVec)

  # convert into a graph
  g <- graph.data.frame(relations, directed=TRUE, vertices=actors)

  # Make the node labels play nice with Gephi
  V(g)$label <- V(g)$name

  # for some reason the dummy row is still generating the "foo" node!
  # a quick way to fix this for now:
  toDel <- which(V(g)$name=="foo" | V(g)$name=="buzz" | V(g)$name=="fizz" | V(g)$name=="bar")
  if (length(toDel)>0) {
    g <- delete.vertices(g, toDel)
  }

  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    # Output the final network to a graphml file, to import directly into Gephi
    currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
    currTime <- gsub(":","_",currTime)
    write.graph(g,paste0(currTime,"_FacebookBimodalNetwork.graphml"),format="graphml")
    cat("Facebook dynamic bimodal network was written to current working directory, with filename:\n")
    cat(paste0(currTime,"_FacebookDynamicBimodalNetwork.graphml"))
  }

  cat("\nDone!\n") ### DEBUG
  flush.console()

  return(g)

}
