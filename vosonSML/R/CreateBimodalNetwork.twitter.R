#' @export
CreateBimodalNetwork.twitter <-
function(x,writeToFile,removeTermsOrHashtags)
{
  from=to=edgeType=timeStamp=tweet_id=NULL # to please the gods of R CMD CHECK

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  if (!missing(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- as.vector(removeTermsOrHashtags) #coerce to vector... to be sure
  }

  if (missing(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- "#fake_hashtag_foobar42_1234567890"
  }

  df <- x # match the variable names (this must be used to avoid warnings in package compilation)

  # convert df to data.table
  df <- data.table(df)

  # Now create the dfBimodalNetwork2, a dataframe of relations between users and hashtags (i.e. user i "tweeted" hashtag j)

  print("Generating Twitter bimodal network...")  ### DEBUG
  flush.console()

  #### ----- NEW WAY ---------

  # for speed we will pre-allocate `dataCombined` to a very large size (more rows than needed)
  # and after everything is finished we will delete the unused rows

  dataCombined <- data.table(
    from = as.character(c(rep("NA_f00",20000000))),
    to = as.character(c(rep("NA_f00",20000000))),
    edgeType = as.character(c(rep("NA_f00",20000000))),
    timeStamp = as.character(c(rep("NA_f00",20000000))),
    tweet_id = as.character(c(rep("NA_f00",20000000)))
  )

  setkey(dataCombined,from) # set the key value of the data table

  nextEmptyRow <- 1 # so we can update rows in `dataCombined` in a relatively efficient way

  # We only need to do the 'hashtag' data (currently)
  for (i in 1:nrow(df)) {

    if (length(df$hashtags_used[[i]]) > 0) { # skip any rows where no hashtags were used

      for (j in 1:length(df$hashtags_used[[i]])) { # for each hashtag in list

        dataCombined[nextEmptyRow, from:= as.character(df$from_user[i][[1]])]
        dataCombined[nextEmptyRow, to := as.character(df$hashtags_used[[i]][j])]
        dataCombined[nextEmptyRow, edgeType := as.character("Used_hashtag")]
        dataCombined[nextEmptyRow, timeStamp := as.character(df$created_at[i][[1]])]
        dataCombined[nextEmptyRow, tweet_id := as.character(df$id[i][[1]])]

        nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`

      }
    }
  }

  # we now delete all the rows at the end of `dataCombined` that are unused
  dataCombined <- dataCombined[edgeType != "NA_f00"] # we just keep the rows that are unchanged from the original dummy data values

  # make a vector of all the unique actors in the network1
  actorsNames <- unique(factor(c(as.character(unique(dataCombined$from)),as.character(unique(dataCombined$to)))))

  relations <- data.frame(
    from=dataCombined$from,
    to=dataCombined$to,
    edgeType=dataCombined$edgeType,
    timeStamp=dataCombined$timeStamp,
    tweet_id=dataCombined$tweet_id)

  suppressWarnings(
    g <- graph.data.frame(relations, directed=TRUE, vertices=actorsNames) # used to be vertices=actors (when it collected user data)
  )

  # Make the node labels play nice with Gephi
  V(g)$label <- V(g)$name

  # remove the search term / hashtags, if user specified it:
  if (removeTermsOrHashtags[1]!="#fake_hashtag_foobar42_1234567890") {
      toDel <- match(tolower(removeTermsOrHashtags),V(g)$name) # we force to lowercase because all terms/hashtags are already converted to lowercase
      toDel <- toDel[!is.na(toDel)] # in case of user error (i.e. trying to delete terms/hashtags that don't exist in the data)
      g <- delete.vertices(g, toDel)
  }

  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    # Output the final network to a graphml file, to import directly into Gephi
    currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
    currTime <- gsub(":","_",currTime)
    write.graph(g,paste0(currTime,"_TwitterBimodalNetwork.graphml"),format="graphml")
    cat("Twitter bimodal network was written to current working directory, with filename:\n")
    cat(paste0(currTime,"_TwitterBimodalNetwork.graphml"))
  }

  cat("\nDone\n") ### DEBUG
  flush.console()

  return(g)

}
