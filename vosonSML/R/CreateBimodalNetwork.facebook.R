#' @export
CreateBimodalNetwork.facebook <-
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

  dataCombinedUNIQUE <- x # match the variable names (this must be used to avoid warnings in package compilation)

  # if `dataCombinedUNIQUE` is a list of dataframes, then need to convert these into one dataframe
  suppressWarnings(
    if (class(dataCombinedUNIQUE)=="list") {
    dataCombinedUNIQUE <- do.call("rbind", dataCombinedUNIQUE)
    }
  )

  #EnsurePackage("igraph")

  cat("\nCreating Facebook bimodal network...\n")

  # make a vector of all the unique actors in the network1
  # usersVec <- rep(c("User"),length(unique(dataCombinedUNIQUE$from)))
  # postsVec <- rep(c("Post"),length(unique(dataCombinedUNIQUE$to)))
  # usersAndPostsVec <- c(usersVec,postsVec)
  # actors <- data.frame(name=unique(factor(c(as.character(unique(dataCombinedUNIQUE$from)),
  # as.character(unique(dataCombinedUNIQUE$to))))),type=usersAndPostsVec)
  actors <- data.frame(name=unique(c(
                    unique(dataCombinedUNIQUE$from),unique(dataCombinedUNIQUE$to)
                   )))

  # make a dataframe of the relations between actors
  # we need a dataframe here because igraph needs it AFAIK

  relations <- data.frame(
    from=dataCombinedUNIQUE$from,
    to=dataCombinedUNIQUE$to,
    edgeType=dataCombinedUNIQUE$edgeType,
    timestamp=dataCombinedUNIQUE$commentTimestamp
    )

  # construct a graph
  g <- graph.data.frame(relations, directed=TRUE, vertices=actors)
  # we will add the vertex 'type' attribute after creating the network
  V(g)$type <- "User" # default to user
  V(g)$type[grep("_",V(g)$name)] <- "Post" # change the posts to post
  # now we also need to add a username attribute for the users
  V(g)$username <- dataCombinedUNIQUE$from_username[match(V(g)$name, dataCombinedUNIQUE$from)]
  # we will just give the posts a username equal to their ID
  V(g)$username[which(is.na(V(g)$username))] <- V(g)$name[which(is.na(V(g)$username))]
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
    cat("Facebook bimodal network was written to current working directory, with filename:\n")
    cat(paste0(currTime,"_FacebookBimodalNetwork.graphml"))
  }

  cat("\nDone!\n") ### DEBUG
  flush.console()

  return(g)

}
