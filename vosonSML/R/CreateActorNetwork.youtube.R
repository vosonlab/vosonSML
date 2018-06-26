#' @export
CreateActorNetwork.youtube <-
function(x,writeToFile) {

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  returnedScrapedComments <- x # match the variable names (this must be used to avoid warnings in package compilation)

  #EnsurePackage("igraph")

  dfRepliesOrParents <- returnedScrapedComments[(which(returnedScrapedComments[,7]!="None" | returnedScrapedComments[,8]!="FALSE")),]

  ## delete rows where user has replied in their own thread (???)
  # Just take care of this when SIMPLIFY the graph (i.e. no self loop edges)?
  # dfRepliesOrParents <- dfRepliesOrParents[-(which(dfRepliesOrParents[,2] == dfRepliesOrParents[,8])),]

  if (nrow(dfRepliesOrParents)==0){
    cat(paste0("\nOops! There are no user interactions to make a network from. \nPlease find video(s) where users have replied or mentioned other users.\nReturning...\n"))
    return()
  }

      usersTemp <- dfRepliesOrParents[,2]
      mentionedUsersTemp <- dfRepliesOrParents[,8]
      numMentions <- c(rep(0,length(mentionedUsersTemp)))

  # create the "mentions network" dataframe (i.e. pairs of users; node i --(mentions)--> node j)
  dfActorNetwork1 <- data.frame(usersTemp, mentionedUsersTemp,numMentions)

  # OK, now extract only the UNIQUE pairs (i.e. rows)
  # But, also create a WEIGHT value for multiple mentions between users
      # NOTE: This edge weights approach might be problematic for TEMPORAL actor networks, because each edge (with weight > 1) represents mentions in tweets at DIFFERENT TIMES.
      # NOTE: A possible workaround could be to include an edge attribute that is a set of timestamp elements, showing the date/time of each unique 'mention'.
      # NOTE: For example, in a temporal visualisation, the first timestamp might 'pop in' the edge to the graph, which then might start to 'fade out' over time (or just 'pop out' of graph after N seconds) if there are no more timestamps indicating activity (i.e. mentions) between the two users.
      # NOTE: So, a 'timestamps' edge attribute could factor into a kind of 'entropy' based approach to evolving the network visually over time.

  # unique pairs:
  # unique_dfActorNetwork1 <- unique(dfActorNetwork1)
    unique_dfActorNetwork1 <- dfActorNetwork1 # DEBUG - DO WE EVEN WANT UNIQUE PAIRS?

  # number of mentions per pair (i.e. edge weight):
  for (i in 1:nrow(unique_dfActorNetwork1)) {
    unique_dfActorNetwork1[i,3] <- sum(usersTemp==unique_dfActorNetwork1[i,1] & mentionedUsersTemp==unique_dfActorNetwork1[i,2])
  }

  # make a vector of all the unique actors in the network1
  actorsNames <- unique(factor(c(as.character(unique(unique_dfActorNetwork1[,1])),as.character(unique(unique_dfActorNetwork1[,2])))))

  # make a dataframe of the relations between actors

  relations <- data.frame(from=unique_dfActorNetwork1[,1],to=unique_dfActorNetwork1[,2],weight=unique_dfActorNetwork1[,3])

  ##### STEP FOUR #####

  # convert into a graph
  g <- graph.data.frame(relations, directed=TRUE, vertices=actorsNames)
  # shouldn't need to simplify the graph, but it can't hurt anyway
  # edit: there could be very specific cases where simplifying is NOT WANTED, e.g. users who mention themselves...?

  # g <- simplify(g)

  # Make the node labels play nice with Gephi
  V(g)$label <- V(g)$name

  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    # Output the final network to a graphml file, to import directly into Gephi
    write.graph(g,paste0(format(Sys.time(), "%a_%b_%d_%X_%Y_%Z"),"_YoutubeActorNetwork.graphml"),format="graphml")
    cat("YouTube actor network was written to current working directory, with filename:\n")
    cat(paste0(format(Sys.time(), "%a_%b_%d_%X_%Y_%Z"),"_YoutubeActorNetwork.graphml"))
  }

  cat("\nDone!\n") ### DEBUG
  flush.console()

  return(g)

}
