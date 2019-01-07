CreateBimodalNetwork.twitter <- function(x, writeToFile = FALSE, removeTermsOrHashtags, verbose = FALSE) {
  
  from <- to <- edge_type <- timestamp <- status_id <- NULL
  
  if (!missing(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- as.vector(removeTermsOrHashtags) # coerce to vector to be sure
  }
  
  if (missing(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- "#fake_hashtag_foobar42_1234567890"
  }
  
  df <- x
  df <- data.table(df)

  df_stats <- networkStats(NULL, "collected tweets", nrow(df))

  # create dfBimodalNetwork2, a dataframe of relations between users and hashtags (i.e. user i "tweeted" hashtag j)
  cat("Generating twitter bimodal network...\n")
  flush.console()

  df_entities <- data.table("entity_id" = character(0), "display_name" = character(0))

  # for speed we will pre-allocate dataCombined to a very large size (more rows than needed)
  # and after everything is finished we will delete the unused rows
  dataCombined <- data.table(
    from = as.character(c(rep("NA_f00", 20000000))),
    to = as.character(c(rep("NA_f00", 20000000))),
    edge_type = as.character(c(rep("NA_f00", 20000000))),
    timestamp = as.character(c(rep("NA_f00", 20000000))),
    status_id = as.character(c(rep("NA_f00", 20000000)))
  )
  
  setkey(dataCombined, from) # set the key value of the data table
  
  nextEmptyRow <- 1 # so we can update rows in 'dataCombined' in a relatively efficient way
  
  # we only need to do the 'hashtag' data (currently)
  count <- 0
  hashtag_count <- 0
  for (i in 1:nrow(df)) {
    if (length(df$hashtags[[i]]) > 0) { # skip any rows where no hashtags were used # hashtags_used
      if (length(df$hashtags[[i]]) == 1 & is.na(df$hashtags[[i]][1])) {
        next 
      }
      
      count <- count + 1
      df_entities <- rbind(df_entities, list(df$user_id[i][[1]], df$screen_name[i][[1]]), stringsAsFactors = FALSE)
      
      for (j in 1:length(df$hashtags[[i]])) { # for each hashtag in list
        
        tag <- paste0("#", df$hashtags[[i]][j])
        
        dataCombined[nextEmptyRow, from:= as.character(df$user_id[i][[1]])]
        dataCombined[nextEmptyRow, to := as.character(tag)]
        dataCombined[nextEmptyRow, edge_type := as.character("hashtag")]
        dataCombined[nextEmptyRow, timestamp := as.character(df$created_at[i][[1]])]
        dataCombined[nextEmptyRow, status_id := as.character(df$status_id[i][[1]])]
        
        df_entities <- rbind(df_entities, list(tag, tag), stringsAsFactors = FALSE)

        hashtag_count = hashtag_count + 1 
        nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`
      }
    }
  }
  df_stats <- networkStats(df_stats, "tweets with hashtags", count, TRUE)
  df_stats <- networkStats(df_stats, "hashtags", hashtag_count, TRUE)
  
  dataCombined <- dataCombined[edge_type != "NA_f00"]
  
  df_entities <- unique(df_entities)
  
  df_stats <- networkStats(df_stats, "nodes", nrow(df_entities))
  df_stats <- networkStats(df_stats, "edges", sum(df_stats$count[df_stats$edge_count == TRUE]))
  if (verbose) {
    networkStats(df_stats, print = TRUE) 
  }
  
  relations <- data.frame(
    from = dataCombined$from,
    to = dataCombined$to,
    edge_type = dataCombined$edge_type,
    timestamp = dataCombined$timestamp,
    status_id = dataCombined$status_id)
  
  g <- graph.data.frame(relations, directed = TRUE, vertices = df_entities)
  
  V(g)$display_name <- ifelse(is.na(V(g)$display_name), paste0("ID:", V(g)$name), V(g)$display_name)
  
  # remove the search term / hashtags, if user specified it:
  if (removeTermsOrHashtags[1] != "#fake_hashtag_foobar42_1234567890") {
    # we force to lowercase because all terms/hashtags are already converted to lowercase
    toDel <- match(tolower(removeTermsOrHashtags), V(g)$name)
    # in case of user error (i.e. trying to delete terms/hashtags that don't exist in the data)
    toDel <- toDel[!is.na(toDel)]
    g <- delete.vertices(g, toDel)
  }
  
  V(g)$label <- V(g)$display_name
  
  if (writeToFile) { writeOutputFile(g, "graphml", "TwitterBimodalNetwork") }
  
  cat("Done.\n")
  flush.console()
  
  return(g)
}
